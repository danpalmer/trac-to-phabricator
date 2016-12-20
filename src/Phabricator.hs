{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}

module Phabricator where

import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Int
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as J
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Text.Encoding (decodeUtf8)
import System.IO.Streams.List (toList)
import Database.MySQL.Base
import Network.Conduit.Client hiding (User, Repo)
import Control.Monad
import Trac
import Debug.Trace
import Config
import Types
import Data.IORef
import qualified Data.IntMap as M
import Data.IntMap ((!))
import qualified Data.HashMap.Strict as H ((!))
import Data.Monoid

import qualified Database.MySQL.Base as M
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B (readFile, ByteString)
import qualified Data.ByteString.Base64 as B (encode)

import Network.Mime

data PhabricatorConnection
  = PC { pcManiphest :: C 'Ticket
       , pcUser :: C 'User
       , pcProject :: C 'Project
       , pcRepo :: C 'Repo }

connectPhab :: IO PhabricatorConnection
connectPhab = do
  cm <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_maniphest" }
  cu <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_user" }
  cp <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_project" }
  cr <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_repository" }
  return (PC cm cu cp cr)

closePhab :: PhabricatorConnection -> IO ()
closePhab (PC (C c1) (C c2) (C c3) (C c4)) = M.close c1 >> M.close c2 >> M.close c3 >> M.close c4

newtype C a = C { getConn :: M.MySQLConn }

data APIPHID a = APIPHID
    { api_phid :: PHID a
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''APIPHID)

data ManiphestPriority = Unbreak | Triage | High | Normal | Low | Wishlist
    deriving (Show)

data PhabricatorUser = PhabricatorUser
    { u_phid         :: UserID
    , u_userName     :: Text
    } deriving (Show)

data PhabricatorProject = PhabricatorProject
    { p_phid        :: ProjectID
    , p_projectName :: Text
    } deriving Show

data ManiphestTicket = ManiphestTicket
    { m_tracn :: Int
    , m_title :: Text
    , m_description :: Maybe Text
    , m_authorPHID :: UserID
    , m_ownerPHID :: Maybe UserID
    , m_cc :: [UserID]
    , m_priority :: ManiphestPriority
    , m_created :: DiffTime
    , m_modified :: DiffTime
--    , m_phid :: Maybe TicketID
    , m_status :: Text
    , m_projects :: [ProjectID] -- Milestone, keywords, type,
    , m_changes :: [ManiphestChange]
    , m_commits :: [ManiphestCommit]
    , m_attachments :: [ManiphestAttachment]
    , m_diffs :: [Int]
    , m_diffHistory :: [ManiphestChange] -- All "differential" field changes
    } deriving (Show)

data ManiphestAttachment = ManiphestAttachment
    { ma_tracn :: Int
    , ma_name :: Text
--    , ma_size :: Text
    , ma_time :: DiffTime
    , ma_desc :: Text
    , ma_author :: UserID } deriving Show

data ManiphestChange = ManiphestChange
  { mc_type   :: MCType
  , mc_created :: DiffTime
  , mc_authorId :: UserID
  } deriving Show

data ManiphestCommit = ManiphestCommit
                     { mc_id :: Text
                     , mc_author :: UserID
                     , mc_time :: DiffTime
                     } deriving Show

data MCType = MCComment Text
            | MCCCRemove [UserID]
            | MCCCAdd [UserID]
            | MCArchitecture Text
            | MCBlockedBy [TicketID]
            | MCComponent Text
            | MCDescription Text
            | MCDifferentialAdd [Int]
            | MCDifferentialRemove [Int]
            | MCDifficulty Text
            | MCFailure Text
            | MCAddKeyword [ProjectID]
            | MCRemoveKeyword [ProjectID]
            | MCMilestone Text
            | MCOS Text
            | MCOwner (Maybe UserID) -- Nothing to unassign
            | MCPatch
            | MCPriority ManiphestPriority
            | MCRelated
            | MCReporter
            | MCResolution Text
            | MCSeverity
            | MCStatus Text
            | MCSummary Text
            | MCTestcase
            | MCType Text
            | MCVersion Text
            | MCWiki Text
            | Dummy Text deriving Show



mysqlToUser :: [MySQLValue] -> Maybe PhabricatorUser
mysqlToUser values =
    case values of
        [x,y] -> PhabricatorUser <$> decodePHID x <*> decodeUserName y
        _ -> Nothing

mysqlToProject :: [MySQLValue] -> Maybe PhabricatorProject
mysqlToProject values =
    case values of
        [x,y] -> PhabricatorProject <$> decodePHID x <*> decodeUserName y
        _ -> Nothing

decodeUserName u = case u of
  MySQLText t -> Just t
  _ -> Nothing


decodePHID p = case p of
  MySQLBytes v -> Just . PHID $ decodeUtf8 v
  _ -> Nothing


mysqlToUsers :: [[MySQLValue]] -> [PhabricatorUser]
mysqlToUsers = mapMaybe mysqlToUser

mysqlToProjects :: [[MySQLValue]] -> [PhabricatorProject]
mysqlToProjects = mapMaybe mysqlToProject


getPhabricatorUsers :: C 'User -> IO [PhabricatorUser]
getPhabricatorUsers (C conn) = do
    (_, rawUsersStream) <- query_ conn "SELECT phid, userName FROM user"
    rawUsers <- toList rawUsersStream
    return $ mysqlToUsers rawUsers

getPhabricatorProjects :: C 'Project -> IO [PhabricatorProject]
getPhabricatorProjects (C conn) = do
  (_, rawProjectStream) <- query_ conn "SELECT phid, name FROM project"
  rawProjects <- toList rawProjectStream
  return $ mysqlToProjects rawProjects



createPhabricatorTickets :: C 'Ticket -> WorkDescription
                         -> [ManiphestTicket] -> IO ()
createPhabricatorTickets conn wd tickets = do
    tm <- ticketQuery conn
    let as = getTickets wd
              $ concatMap (createPhabricatorTicketAction tm conn) tickets
    doActions as
    fixSubscribers conn

data WorkDescription = Exact Int | UpTo Bool Int | AllT | Range Int Int
                     | FromTime Integer

pattern DownTo n = UpTo True n


getTickets :: WorkDescription -> [Action] -> [Action]
getTickets (Exact n) ts = filter ((== n) . actionTicket) ts
getTickets (UpTo rev n) ts  = take n (if rev then reverse ts else ts)
getTickets AllT ts = ts
getTickets (Range low up) ts =
    filter (\t -> low <= actionTicket t && actionTicket t <= up) ts
getTickets (FromTime n) ts =
    let t = picosecondsToDiffTime n
    in filter (\a -> actionTime a >= t
                    && actionType a /= TicketCreate ) ts

doActions :: [Action] -> IO ()
doActions as = do
    let as' = sort as
    print (length as')
    mapM_ (\(n, a) -> print n >> getAction a)(zip [0..] as')


instance Ord Action where
  compare (Action n at t _) (Action m at' t' _) =
    compare (at, n) (at', m) <> compare t t'

instance Eq Action where
    a == b = compare a b == EQ

data Action = Action {
            actionTicket :: Int
            , actionType :: ActionType
            , actionTime :: DiffTime
            , getAction :: (IO ())}

instance Show Action where
    show = printAction

printAction :: Action -> String
printAction (Action at n d _) = show (n, at, d)

data ActionType = TicketCreate | TicketUpdate deriving (Eq, Show, Ord)

mkTicketCreate :: Int -> DiffTime -> IO () -> Action
mkTicketCreate n t ac = Action n TicketCreate t ac

mkTicketUpdate :: Int -> DiffTime -> IO () -> Action
mkTicketUpdate n t ac = Action n TicketUpdate t ac

ticketQuery :: C 'Ticket -> IO TicketMap
ticketQuery (C conn) = do
  rawres <- snd <$> query_ conn "SELECT id, phid FROM maniphest_task"
  rawList <- toList rawres
  let res = map convert rawList
  newIORef (M.fromList res)
  where
    convert :: [MySQLValue] -> (Int, TicketID)
    convert [MySQLInt32U n, MySQLBytes v]
      =  (fromIntegral n, PHID $ decodeUtf8 v)
    convert e = error (show e)


type TicketMap = IORef (M.IntMap TicketID)

badTickets :: [Int]
badTickets = [8539]

createPhabricatorTicketAction :: TicketMap -> C 'Ticket -> ManiphestTicket
                              -> [Action]
createPhabricatorTicketAction tm conn ticket = do
    traceShowM (T.concat [T.pack (show $ m_tracn ticket),": ", m_title ticket])
    if m_tracn ticket `elem` badTickets
      then []
      else
        mkTicketCreate
          (m_tracn ticket)
          (m_created ticket)
          (mkTicket conn ticket tm >> updatePhabricatorTicket tm conn ticket)
        : doTransactions tm conn ticket

mkTicket :: C 'Ticket -> ManiphestTicket -> TicketMap -> IO ()
mkTicket conn t tm = do
  APIPHID pid <- fromConduitResult <$> callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs t)

  let
    diffList :: [Int]
    diffList = m_diffs t
  editDependencies conn pid "dependsOnDiffs" ([ "add" .= [1::Int] ] :: [J.Pair])
                   (m_authorPHID t) (m_created t)
  modifyIORef tm (M.insert (m_tracn t) pid)

editDependencies :: ToJSON v
                 => C 'Ticket
                 -> TicketID
                 -> Text
                 -> v
                 -> UserID
                 -> DiffTime
                 -> IO ()
editDependencies conn tid field v uid t = do
  (res ::  Object) <- fromConduitResult <$>
    callConduitPairs conduit "maniphest.editdependencies"
      [ "taskPHID" .= tid
      , field .= v
      , "author" .= uid ]

  let ts = case J.parseEither transactionListParser res of
            Left e -> error e
            Right r -> r
  traceShowM ts
  mapM_ (fixTransactionInformation conn t) ts

fromConduitResult :: ConduitResponse a -> a
fromConduitResult (ConduitResult a) = a
fromConduitResult (ConduitError code info) = error (show code ++ show info)

buildTransaction :: ManiphestChange -> Maybe Value
buildTransaction = doOne
  where
    doOne :: ManiphestChange -> Maybe Value
    doOne ManiphestChange{..} =
      case mc_type of
        MCComment c -> Just $ mkTransaction "comment" c
        MCCCRemove cs   -> Just $ mkTransaction "subscribers.remove" cs
        MCCCAdd cs   -> Just $ mkTransaction "subscribers.add" cs
        MCArchitecture v -> Just $ mkTransaction "custom.ghc:architecture" v
        MCBlockedBy bs   -> Nothing --Just $ mkTransaction "parent" bs
        MCComponent c    -> Just $ mkTransaction "custom.ghc:failure" c
        MCDescription d  -> Just $ mkTransaction "description" d
        MCDifferentialRemove d -> error "DiffRemove"
        MCDifferentialAdd d -> error "DiffAdd"
        MCDifficulty d   -> Just $ mkTransaction "custom.ghc:difficulty" d
        MCFailure f      -> Just $ mkTransaction "custom.ghc:failure" f
--        MCKeywords pids  -> Just $ mkTransaction "projects.set" pids
        MCMilestone m    -> Just $ mkTransaction "custom.ghc:milestone" m
        MCOS os          -> Just $ mkTransaction "custom.ghc:os" os
        MCOwner mown     -> Just $ mkTransaction "owner" mown
        MCPatch          -> Nothing -- Junk field
        MCPriority p     -> Just $ mkTransaction "priority" (show (priorityToInteger p))
        MCRelated        -> Nothing -- Unsure
        MCReporter       -> Nothing -- Handled and old
        MCResolution r   -> Just $ mkTransaction "status" r -- Resolutions are actually closed statuses
        MCSeverity       -> Nothing -- Not used
        MCStatus s       -> Just $ mkTransaction "status" s
        MCSummary s      -> Just $ mkTransaction "title" s
        MCTestcase       -> Nothing --TODO if important
        MCType t         -> Just $ mkTransaction "custom.ghc:type" t
        MCVersion v      -> Just $ mkTransaction "custom.ghc:version" v
        MCWiki w         -> Nothing -- TO implement this field
        -- We try to remove the old one and add the new one.
        MCAddKeyword new -> Just $ mkTransaction    "projects.add"    new
        MCRemoveKeyword old -> Just $ mkTransaction "projects.remove" old
        Dummy diag -> Nothing

mkTransaction :: ToJSON a => Text -> a -> Value
mkTransaction ty val = object [ "type" .= ty
                              , "value" .= val ]


ticketToConduitPairs :: ManiphestTicket -> [J.Pair]
ticketToConduitPairs ticket =
    [ "title" .= m_title ticket
    , "description"  .= m_description ticket
    , "ownerPHID" .= m_ownerPHID ticket
    , "priority" .= priorityToInteger (m_priority ticket)
    , "ccPHIDs" .= m_cc ticket
    , "projectPHIDs" .= m_projects ticket
    , "user" .= [m_authorPHID ticket]
    ]


-- Two transactions, upload the file and post a comment saying the
-- attachment was added.
attachmentTransaction :: TicketMap -> C 'Ticket
                      -> ManiphestTicket -> ManiphestAttachment -> Action
attachmentTransaction tm conn n a@ManiphestAttachment{..} =
  mkTicketUpdate ma_tracn ma_time $ do
    aid <- uploadAttachment a
    let attachmentChange :: ManiphestChange
        attachmentChange = ManiphestChange (MCComment attachmentComment)
                                          ma_time
                                          ma_author
        attachmentComment = T.unwords ["Attachment", aid, "added"]
    fromMaybe (return ()) (doOneTransaction tm conn n attachmentChange)

commitTransaction :: TicketMap -> C 'Ticket -> ManiphestTicket
                  -> ManiphestCommit -> Action
commitTransaction tm conn n ManiphestCommit{..} =
  mkTicketUpdate (m_tracn n) mc_time $ do
   tid <- getTicketID n tm
   editDependencies conn tid "dependsOnCommits" [mc_id] botUser mc_time

differentialTransaction :: TicketMap -> C 'Ticket -> ManiphestTicket
                        -> ManiphestChange -> Action
differentialTransaction tm conn n ManiphestChange{..} =
  case mc_type of
    MCDifferentialRemove rs -> traceShow ("REMOVE", rs, m_tracn n)
                                  (mkTicketUpdate (m_tracn n) 0 (return ()))
    MCDifferentialAdd rs ->
      mkTicketUpdate (m_tracn n) mc_created $ do
        tid <- getTicketID n tm
        editDependencies conn tid "dependsOnDiffs" rs mc_authorId mc_created
    c -> error (show ("diffTrans", c))




doTransactions :: TicketMap -> C 'Ticket -> ManiphestTicket -> [Action]
doTransactions tm conn mt =
  let cs = m_changes mt
  in
    map (attachmentTransaction tm conn mt) (m_attachments mt)
     ++ map (commitTransaction tm conn mt) (m_commits mt)
     ++ map (differentialTransaction tm conn mt) (m_diffHistory mt)
     ++ mapMaybe (mkOneAction tm conn mt) cs

mkOneAction :: TicketMap -> C 'Ticket -> ManiphestTicket -> ManiphestChange -> Maybe Action
mkOneAction tm conn n mc =
  mkTicketUpdate (m_tracn n) (mc_created mc) <$>
    doOneTransaction tm conn n mc

-- We have to do each one individually with 10000s of API calls to make
-- sure we get exactly 0 or 1 transactions for each update so we can match
-- the author and time information correctly.
doOneTransaction :: TicketMap -> C 'Ticket -> ManiphestTicket -> ManiphestChange -> Maybe (IO ())
doOneTransaction tm conn n mc =
  case buildTransaction mc of
    Nothing -> Nothing
    Just v  -> Just $ do
      traceM (show $ (m_tracn n, mc_created mc, take 50 (show mc)))
      tid <- getTicketID n tm
      res <- fromConduitResult <$> callConduitPairs conduit "maniphest.edit"
                [ "objectIdentifier" .= tid
                , "transactions" .= [v]
                , "author" .= mc_authorId mc ]
      let ts = case J.parseEither transactionParser res of
            Left e -> error e
            Right r -> r
      traceShowM ts
      mapM_ (fixTransactionInformation conn (mc_created mc)) ts


transactionParser :: Object -> J.Parser [TransactionID]
transactionParser o = do
  ts <- o .: "transactions"
  J.listParser (withObject "" (.: "phid")) ts

transactionListParser :: Object -> J.Parser [TransactionID]
transactionListParser o = do
  traceShowM o
  ts <- o .: "transactions"
  J.listParser parseJSON ts
--  parseJSON ts



-- Need to go into two tables,  phabricator_manifest_transaction and
-- phabricator_manifest_comment
-- I think this is just used now to set the correct time.. yay!
fixTransactionInformation ::
                         C 'Ticket
                      -> DiffTime
                      -> TransactionID
                      -> IO ()
fixTransactionInformation (C conn) date (PHID tid) =
  let fix1 = "UPDATE maniphest_transaction SET dateCreated=?, dateModified=? WHERE phid=?"
      values1 = [ MySQLInt64 $ convertTime date, MySQLInt64 $ convertTime date, MySQLText tid]
  in
    void $ execute conn fix1 values1




priorityToInteger :: ManiphestPriority -> Integer
priorityToInteger p =
    case p of
        Unbreak -> 100
        Triage -> 90
        High -> 80
        Normal -> 50
        Low -> 25
        Wishlist -> 0


updatePhabricatorTicket :: TicketMap -> C 'Ticket -> ManiphestTicket -> IO ()
updatePhabricatorTicket tm (C conn) ticket = do
    let q = "UPDATE maniphest_task SET id=?, dateCreated=?, dateModified=?, authorPHID=? WHERE phid=?;"
        -- Some queries go from this separate table rather than the actual information in the ticket.
        q2 = "UPDATE maniphest_transaction SET dateCreated=?, authorPHID=? WHERE objectPHID=?"
    t <- getTicketID ticket tm
    let values = ticketToUpdateTuple t ticket
    execute conn q values
    execute conn q2 [values !! 1 , values !! 3, values !! 4]
    return ()

fixSubscribers :: C 'Ticket -> IO ()
fixSubscribers (C conn) = do
        -- A subscriber is automatically added
        -- This is where the notification a subscriber is added is
        -- populated from. We have to be careful to just remove the bot
        -- user ID. Doing any action with the API adds you as a subscriber
        -- so you should remove the edges at the END as well.
  let   q3 = "DELETE FROM maniphest_transaction WHERE transactionType=? AND newValue like '%cv4luanhibq47r6o2zrb%' "
        -- This is where the subscribers info is populated from
        q4 = "DELETE FROM edge WHERE dst=?"

  execute conn q3 [MySQLText "core:subscribers" ]
  execute conn q4 [MySQLText $ unwrapPHID botUser]
  return ()



ticketToUpdateTuple :: TicketID -> ManiphestTicket ->  [MySQLValue]
ticketToUpdateTuple (PHID t) ticket =
            [ MySQLInt64 $ fromIntegral (m_tracn ticket)
            , MySQLInt64 $ convertTime (m_created ticket)
            , MySQLInt64 $ convertTime (m_modified ticket)
            , MySQLText $ unwrapPHID (m_authorPHID ticket)
            , MySQLText t
            ]

getTicketID :: ManiphestTicket -> TicketMap -> IO TicketID
getTicketID m tm = (! m_tracn m) <$> readIORef tm

deleteProjectInfo :: C 'Project -> IO ()
deleteProjectInfo (C conn) = void $  do
  execute_ conn "DELETE FROM project"
  execute_ conn "DELETE FROM project_transaction"
  execute_ conn "DELETE FROM project_column"
  execute_ conn "DELETE FROM project_columnposition"
  execute_ conn "DELETE FROM project_columntransaction"
  execute_ conn "DELETE FROM project_transaction"
  execute_ conn "DELETE FROM edge"
  execute_ conn "DELETE FROM project_slug"

deleteTicketInfo :: C 'Ticket -> IO ()
deleteTicketInfo (C conn) = void $ do
  execute_ conn "DELETE FROM maniphest_task"
  execute_ conn "DELETE FROM maniphest_transaction"
  execute_ conn "DELETE FROM maniphest_transaction_comment"
  execute_ conn "DELETE FROM edge"

createProject :: Text -> IO (Maybe ProjectID)
createProject "" = return Nothing
createProject i@kw = do
  response <- callConduitPairs conduit "project.create" ["name" .= kw
                    --                                    ,"icon" .= keywordTypeToIcon ty
                      --                                  ,"colour" .= keywordTypeToColour ty
                                                        -- The API call
                                                        -- fails if you
                                                        -- don't pass this
                                                        -- param
                                                        , "members" .= ([] :: [()])]
  traceShowM (i, response :: ConduitResponse (APIPHID 'Project))
  case response of
    ConduitResult (APIPHID a) -> return (Just a)
    ConduitError {} -> do
      traceShowM response
      return Nothing

keywordTypeToIcon :: KeywordType -> Text
keywordTypeToIcon t =
  case t of
    Milestone -> "release"
    _ -> "project"

keywordTypeToColour :: KeywordType -> Text
keywordTypeToColour t = "red"

data SubOrMil = Sub | Mil

addSubproject :: ProjectID -> SubOrMil -> Text ->  IO ()
addSubproject phid sorm name = do
  (response :: ConduitResponse Value) <- callConduitPairs conduit "project.edit"
                [ "transactions" .= [mkTransaction (tt sorm) phid
                                    , mkTransaction "name" name ]
                ]
  traceShowM response
  return ()
  where
    tt Sub = "parent"
    tt Mil = "milestone"


convertTime :: DiffTime -> Int64
convertTime t = fromIntegral $ diffTimeToPicoseconds t `div` 1000000

-- Attachments
-- If a file is an image we upload it to FILE
-- If it is a text attachment we upload it to PASTE
--
-- Returns the name we want to refer to it as.
uploadAttachment :: ManiphestAttachment -> IO Text
uploadAttachment a =
  let mime = C8.unpack $ defaultMimeLookup (ma_name a)
  in case mime of
       't':'e':'x':'t':_ -> uploadPasteAttachment a
       _ -> uploadFileAttachment a

uploadPasteAttachment :: ManiphestAttachment -> IO Text
uploadPasteAttachment a = do
  fileData <- T.readFile filePath
  (response :: Object) <- fromConduitResult <$>
    callConduitPairs conduit "paste.create"
      [ "content" .= fileData
      , "title"   .= fileName
      ]
  return ((\(String s) -> s) $ response H.! "objectName")
  where
    filePath :: String
    filePath = T.unpack (maniphestAttachmentToPath a)

    fileName :: Text
    fileName = ma_name a

uploadFileAttachment :: ManiphestAttachment -> IO Text
uploadFileAttachment a = do
  fileData <- TE.decodeUtf8 . B.encode <$> B.readFile filePath
  phid :: Text <- fromConduitResult <$>
    callConduitPairs conduit "file.upload"
      [ "data_base64" .= fileData
      , "name"        .= fileName
      ]
  (fileInfo :: Object) <- fromConduitResult <$>
    callConduitPairs conduit "file.info"
      [ "phid" .= phid ]
  return ((\(String s) -> s) $ fileInfo H.! "objectName")
  where
    filePath :: String
    filePath = T.unpack (maniphestAttachmentToPath a)

    fileName :: Text
    fileName = ma_name a

maniphestAttachmentToPath (ManiphestAttachment{..}) =
  mkAttachmentPath ma_tracn ma_name

-- I don't know what is meant to be in these fields
--createFakeRevisions :: Int -> IO ()
--createFakeRevisions n = forM_ [0..n] $ do
--  callConduitPairs conduit "differential.creatediff"
--    [ "title" .= "asdasdas"


