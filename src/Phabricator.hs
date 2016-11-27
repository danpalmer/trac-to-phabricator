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
import Data.Maybe (mapMaybe)
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
import Network.Conduit.Client hiding (User)
import Control.Monad
import Trac
import Debug.Trace
import Config
import Types

import qualified Trac.Convert as T
import qualified Database.MySQL.Base as M

data PhabricatorConnection
  = PC { pcManiphest :: C 'Ticket , pcUser :: C 'User, pcProject :: C 'Project }

connectPhab :: IO PhabricatorConnection
connectPhab = do
  cm <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_maniphest" }
  cu <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_user" }
  cp <- C <$> M.connect phabConnectInfo { ciDatabase = "bitnami_phabricator_project" }
  return (PC cm cu cp)

closePhab :: PhabricatorConnection -> IO ()
closePhab (PC (C c1) (C c2) (C c3)) = M.close c1 >> M.close c2 >> M.close c3

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
    }

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
    , m_changes :: [ManiphestChange]
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
            | MCKeywords [ProjectID]
            | MCMilestone Text
            | MCOS Text
            | MCOwner UserID
            | MCPatch
            | MCPriority ManiphestPriority
            | MCRelated
            | MCReporter
            | MCResolution
            | MCSeverity
            | MCStatus Text
            | MCSummary Text
            | MCTestcase
            | MCType Text
            | MCVersion Text
            | MCWiki Text
            | Dummy deriving Show



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
        decodePHID p = case p of
            MySQLBytes v -> Just . PHID $ decodeUtf8 v
            _ -> Nothing
        decodeUserName u = case u of
            MySQLText t -> Just t
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
              res <- doTransactions conn (api_phid phidResponse) ticket
            -- Make sure to do this last
              updatePhabricatorTicket conn (ticket {m_phid = Just (api_phid phidResponse)})
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
        MCKeywords pids  -> Just $ mkTransaction "projects.set" pids
        MCMilestone m    -> Just $ mkTransaction "custom.ghc:milestone" m
        MCOS os          -> Just $ mkTransaction "custom.ghc:os" os
        MCOwner mown     -> Just $ mkTransaction "owner" mown
        MCPatch          -> Nothing -- Junk field
        MCPriority p     -> Just $ mkTransaction "priority" (show (priorityToInteger p))
        MCRelated        -> Nothing -- Unsure
        MCReporter       -> Nothing -- Handled and old
        MCResolution     -> Nothing
        MCSeverity       -> Nothing -- Not used
        MCStatus s       -> Just $ mkTransaction "status" s
        MCSummary s      -> Just $ mkTransaction "title" s
        MCTestcase       -> Nothing --TODO if important
        MCType t         -> Just $ mkTransaction "custom.ghc:type" t
        MCVersion v      -> Just $ mkTransaction "custom.ghc:version" v
        MCWiki w         -> Nothing -- TO implement this field
        Dummy -> Nothing

mkTransaction :: ToJSON a => Text -> a -> Value
mkTransaction ty val = object [ "type" .= ty
                              , "value" .= val ]


ticketToConduitPairs :: ManiphestTicket -> [J.Pair]
ticketToConduitPairs ticket =
    [ "title" .= m_title ticket
    , "description"  .= m_description ticket
--    , "ownerPHID" .= m_ownerPHID ticket
    , "priority" .= priorityToInteger (m_priority ticket)
--    , "ccPHIDs" .= m_cc ticket
--    , "projectPHIDs" .= []  -- ["PHID-PROJ-qo3k34ztcwlndlg7pzkb" :: Text]
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
    let q = "UPDATE maniphest_task SET dateCreated=?, dateModified=?, status=?, authorPHID=? WHERE phid=?;"
        -- Some queries go from this separate table rather than the actual information in the ticket.
        q2 = "UPDATE maniphest_transaction SET dateCreated=? WHERE objectPHID=? AND transactionType='status'"
        -- A subscriber is automatically added
        -- This is where the notification a subscriber is added is
        -- populated from
        q3 = "DELETE FROM maniphest_transaction WHERE transactionType=? AND objectPHID=?"
        -- This is where the subscribers info is populated from
        q4 = "DELETE FROM edge WHERE src=?"
    case ticketToUpdateTuple ticket of
      Just values -> do  execute conn q values
                         execute conn q2 [head values, values !! 4]
        --                 execute conn q3 [MySQLText "core:subscribers", values !! 4]
        --                 execute conn q4 [values !! 4]
                         return ()

      Nothing -> return ()


ticketToUpdateTuple :: ManiphestTicket -> Maybe [MySQLValue]
ticketToUpdateTuple ticket =
    case m_phid ticket of
        Just (PHID t) -> Just
            [ MySQLInt64 $ convertTime (m_created ticket)
            , MySQLInt64 $ convertTime (m_modified ticket)
            , MySQLText $ m_status ticket
            , MySQLText $ unwrapPHID (m_authorPHID ticket)
            , MySQLText t
            ]
        Nothing -> Nothing

createProject :: Text -> IO ProjectID
createProject t = do
  response <- callConduitPairs conduit "project.create" ["name" .= t
                                                        -- The API call
                                                        -- fails if you
                                                        -- don't pass this
                                                        -- param
                                                        , "members" .= ([] :: [()])]
  traceShowM (response :: ConduitResponse (APIPHID 'Project))
  case response of
    ConduitResult (APIPHID a) -> return a
    ConduitError {} -> error (show response)


convertTime :: DiffTime -> Int64
convertTime t = fromIntegral $ diffTimeToPicoseconds t `div` 1000000

