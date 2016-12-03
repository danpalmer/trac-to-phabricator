{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phabricator where

import GHC.Generics (Generic)
import Data.Int
import Data.Maybe (mapMaybe, fromJust)
import Data.Either (lefts, rights)
import Data.Text (Text)
import qualified Data.Text as T
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

import qualified Trac.Convert as T
import qualified Database.MySQL.Base as M

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
    , m_phid :: Maybe TicketID
    , m_status :: Text
    , m_projects :: [ProjectID] -- Milestone, keywords, type,
    , m_changes :: [ManiphestChange]
    , m_commits :: [CommitID]
    } deriving (Show)

data ManiphestChange = ManiphestChange
  { mc_type   :: MCType
  , mc_created :: DiffTime
  , mc_authorId :: UserID
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
createPhabricatorTickets conn tickets =
    mapM_ (createPhabricatorTicket conn) tickets


badTickets = [8539]

createPhabricatorTicket :: C 'Ticket -> ManiphestTicket -> IO (Either Text ())
createPhabricatorTicket conn ticket = do
    traceShowM (T.concat [T.pack (show $ m_tracn ticket),": ", m_title ticket])
    if m_tracn ticket `elem` badTickets
      then return (Left "BadTicket")
      else do
        response <- callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs ticket)
        case response of
            ConduitResult phidResponse -> do
              updatePhabricatorTicket conn (ticket {m_phid = Just (api_phid phidResponse)})
              res <- doTransactions conn (api_phid phidResponse) ticket
              fixSubscribers conn (ticket {m_phid = Just (api_phid phidResponse)})
            -- Make sure to do this last
              return (Right ())
            ConduitError code info -> do
              traceShowM (m_title ticket, code, info)
              return $ Left (code `T.append` info)

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

doTransactions :: C 'Ticket -> TicketID -> ManiphestTicket -> IO ()
doTransactions conn tid mt = do
  let cs = m_changes mt
  mapM_ (doOneTransaction conn tid) cs

-- We have to do each one individually with 10000s of API calls to make
-- sure we get exactly 0 or 1 transactions for each update so we can match
-- the author and time information correctly.
doOneTransaction :: C 'Ticket -> TicketID -> ManiphestChange -> IO ()
doOneTransaction conn tid mc = do
  case buildTransaction mc of
    Nothing -> return ()
    Just v  -> do
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

transactionParser :: Object -> J.Parser [(TransactionID)]
transactionParser o = do
  ts <- o .: "transactions"
  J.listParser (withObject "" ((.: "phid"))) ts
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


updatePhabricatorTicket :: C 'Ticket -> ManiphestTicket -> IO ()
updatePhabricatorTicket (C conn) ticket = do
    let q = "UPDATE maniphest_task SET id=?, dateCreated=?, dateModified=?, authorPHID=? WHERE phid=?;"
        -- Some queries go from this separate table rather than the actual information in the ticket.
        q2 = "UPDATE maniphest_transaction SET dateCreated=?, authorPHID=? WHERE objectPHID=?"
    case ticketToUpdateTuple ticket of
      Just values -> do  execute conn q values
                         execute conn q2 [values !! 1 , values !! 3, values !! 4]
                         return ()

      Nothing -> return ()

fixSubscribers :: C 'Ticket -> ManiphestTicket -> IO ()
fixSubscribers (C conn) ticket = do
        -- A subscriber is automatically added
        -- This is where the notification a subscriber is added is
        -- populated from. We have to be careful to just remove the bot
        -- user ID. Doing any action with the API adds you as a subscriber
        -- so you should remove the edges at the END as well.
  let   q3 = "DELETE FROM maniphest_transaction WHERE transactionType=? AND objectPHID=? AND newValue like '%cv4luanhibq47r6o2zrb%' "
        -- This is where the subscribers info is populated from
        q4 = "DELETE FROM edge WHERE dst=?"
  case ticketToUpdateTuple ticket of
      Just values -> do  execute conn q3 [MySQLText "core:subscribers"
                                         , values !! 4
                                         ]
                         execute conn q4 [MySQLText $ unwrapPHID botUser]
                         return ()

      Nothing -> return ()


ticketToUpdateTuple :: ManiphestTicket -> Maybe [MySQLValue]
ticketToUpdateTuple ticket =
    case m_phid ticket of
        Just (PHID t) -> Just
            [ MySQLInt64 $ fromIntegral (m_tracn ticket)
            , MySQLInt64 $ convertTime (m_created ticket)
            , MySQLInt64 $ convertTime (m_modified ticket)
            , MySQLText $ unwrapPHID (m_authorPHID ticket)
            , MySQLText t
            ]
        Nothing -> Nothing

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
  traceShowM (i, (response :: ConduitResponse (APIPHID 'Project)))
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
