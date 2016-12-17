{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Phabricator where

import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Int
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.Either (lefts, rights)
import Data.Text (Text)
import Data.List
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as J
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
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

import qualified Trac.Convert as T
import qualified Database.MySQL.Base as M

import qualified Data.ByteString as B (readFile, ByteString)
import qualified Data.ByteString.Char8 as C8
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
            | MCCC [UserID]
            | MCArchitecture Text
            | MCBlockedBy [TicketID]
            | MCComponent Text
            | MCDescription Text
            | MCDifferential [DiffID]
            | MCDifficulty Text
            | MCFailure Text
            | MCAddKeyword [ProjectID]
            | MCRemoveKeyword [ProjectID]
            | MCMilestone Text
            | MCOS Text
            | MCOwner UserID
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

    where
        decodePHID p = case p of
            MySQLBytes v -> Just . PHID $ decodeUtf8 v
            _ -> Nothing
        decodeUserName u = case u of
            MySQLText t -> Just t
            _ -> Nothing

mysqlToProject :: [MySQLValue] -> Maybe PhabricatorProject
mysqlToProject values =
    case values of
        [x,y] -> PhabricatorProject <$> decodePHID x <*> decodeUserName y
        _ -> Nothing

    where
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



createPhabricatorTickets :: C 'Ticket -> [ManiphestTicket] -> IO ()
createPhabricatorTickets conn tickets = do
    tm <- newIORef M.empty
    let as = concatMap (createPhabricatorTicketAction tm conn) tickets
    doActions as
    fixSubscribers conn

doActions :: [Action] -> IO ()
doActions as = do
    let as' = sort as
    print as'
    mapM_ getAction as'


instance Ord Action where
  compare (Action at t _) (Action at' t' _) = compare at at' <> compare t t'

instance Eq Action where
    a == b = a == b

instance Ord ActionType where
  compare (TicketCreate n) (TicketCreate m) = compare n m
  compare (TicketCreate _) _ = LT
  compare TicketUpdate TicketUpdate = EQ
  compare TicketUpdate _ = GT

instance Eq ActionType where
    a == b = a == b

data Action = Action ActionType DiffTime (IO ())

instance Show Action where
    show = printAction

printAction :: Action -> String
printAction (Action at d _) = show (at, d)

getAction :: Action -> IO ()
getAction (Action _ _ io) = io

data ActionType = TicketCreate Int | TicketUpdate deriving Show

mkTicketCreate :: Int -> DiffTime -> IO () -> Action
mkTicketCreate n t ac = Action (TicketCreate n) t ac

mkTicketUpdate :: DiffTime -> IO () -> Action
mkTicketUpdate t ac = Action TicketUpdate t ac


type TicketMap = IORef (M.IntMap TicketID)

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
          (mkTicket ticket tm >> updatePhabricatorTicket tm conn ticket)
        : doTransactions tm conn ticket

mkTicket :: ManiphestTicket -> TicketMap -> IO ()
mkTicket t tm = do
  APIPHID pid <- fromConduitResult <$> callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs t)
  modifyIORef tm (M.insert (m_tracn t) pid)

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
        MCCC cs -> Just $ mkTransaction "subscribers.set" cs
        MCArchitecture v -> Just $ mkTransaction "custom.ghc:architecture" v
        MCBlockedBy bs   -> Nothing --Just $ mkTransaction "parent" bs
        MCComponent c    -> Just $ mkTransaction "custom.ghc:failure" c
        MCDescription d  -> Just $ mkTransaction "description" d
        MCDifferential d -> Nothing --Just $ mkTransaction "" -- Add edge
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
    ]


-- Two transactions, upload the file and post a comment saying the
-- attachment was added.
attachmentTransaction :: TicketMap -> C 'Ticket
                      -> ManiphestTicket -> ManiphestAttachment -> Action
attachmentTransaction tm conn n a@ManiphestAttachment{..} =
  mkTicketUpdate ma_time $ do
    aid <- uploadAttachment a
    let attachmentChange :: ManiphestChange
        attachmentChange = ManiphestChange (MCComment attachmentComment)
                                          ma_time
                                          ma_author
        attachmentComment = T.unwords ["Attachment", aid, "added"]
    fromMaybe (return ()) (doOneTransaction tm conn n attachmentChange)

commitTransaction :: TicketMap -> C 'Ticket -> ManiphestTicket
                  -> ManiphestCommit -> Maybe Action
commitTransaction tm conn n ManiphestCommit{..} =
  mkOneAction tm conn n (ManiphestChange (MCComment commitComment)
                                  mc_time
                                  mc_author )
  where
    commitComment = T.unwords ["This ticket was mentioned in", mc_id]




doTransactions :: TicketMap -> C 'Ticket -> ManiphestTicket -> [Action]
doTransactions tm conn mt =
  let cs = m_changes mt
  in
    map (attachmentTransaction tm conn mt) (m_attachments mt)
     ++ mapMaybe (commitTransaction tm conn mt) (m_commits mt)
     ++ mapMaybe (mkOneAction tm conn mt) cs

mkOneAction :: TicketMap -> C 'Ticket -> ManiphestTicket -> ManiphestChange -> Maybe Action
mkOneAction tm conn n mc =
  mkTicketUpdate (mc_created mc) <$>
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
      rawres <- callConduitPairs conduit "maniphest.edit"
                [ "objectIdentifier" .= tid
                , "transactions" .= [v] ]
      let res = case rawres of
                  ConduitResult r -> r
                  _ -> error (show rawres)
      let ts = case J.parseEither transactionParser res of
            Left e -> error e
            Right r -> r
      traceShowM ts
      case ts of
        -- Uncomment to debug
        ts -> mapM_ (fixTransactionInformation conn (mc_authorId mc) (mc_created mc)) ts
        -- Exactly one, do the transaction update
        [t] -> fixTransactionInformation conn (mc_authorId mc) (mc_created mc) t
        -- Zero, the edit had no effect
        []  -> return ()
        -- More than one, bad, better to abort
        _  -> error (show ts)

{-
isComment :: ManiphestChange -> Bool
isComment ManiphestChange{mc_type = "comment"} = True
isComment _ = False
-}

transactionParser :: Object -> J.Parser [TransactionID]
transactionParser o = do
  ts <- o .: "transactions"
  J.listParser (withObject "" (.: "phid")) ts
--  parseJSON ts



-- Need to go into two tables,  phabricator_manifest_transaction and
-- phabricator_manifest_comment
fixTransactionInformation ::
                         C 'Ticket
                      -> UserID
                      -> DiffTime
                      -> TransactionID
                      -> IO ()
fixTransactionInformation (C conn) (PHID maid) date (PHID tid) =
  let fix1 = "UPDATE maniphest_transaction SET dateCreated=?, dateModified=?, authorPHID=? WHERE phid=?"
      fix2 = "UPDATE maniphest_transaction_comment SET authorPHID=? WHERE transactionPHID=?"
      values1 = [ MySQLInt64 $ convertTime date, MySQLInt64 $ convertTime date, MySQLText maid, MySQLText tid]
      values2 = [values1 !! 2, values1 !! 3]
  in
    void $ execute conn fix1 values1 >> execute conn fix2 values2




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

lookupCommitID :: C 'Repo -> Text -> IO CommitID
lookupCommitID (C conn) t = do
  [[rs]] <- toList . snd =<< query conn "SELECT phid FROM repository_commit WHERE commitIdentifier=?" [MySQLText t]
  traceShowM rs
  return (fromJust $ decodePHID rs)

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

