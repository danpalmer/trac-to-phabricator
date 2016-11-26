{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Phabricator where

import GHC.Generics
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
import Network.Conduit.Client
import Control.Monad
import Trac
import Debug.Trace
import Config
import Types

import qualified Trac.Convert as T



data APIPHID = APIPHID
    { api_phid :: TicketID
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''APIPHID)

data ManiphestPriority = Unbreak | Triage | High | Normal | Low | Wishlist
    deriving (Show)

data PhabricatorUser = PhabricatorUser
    { u_phid         :: UserID
    , u_userName     :: Text
    } deriving (Show)

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


mysqlToUsers :: [[MySQLValue]] -> [PhabricatorUser]
mysqlToUsers = mapMaybe mysqlToUser


getPhabricatorUsers :: IO [PhabricatorUser]
getPhabricatorUsers = do
    conn <- connect (phabConnectInfo { ciDatabase = "bitnami_phabricator_user" })
    (_, rawUsersStream) <- query_ conn "SELECT phid, userName FROM user"
    rawUsers <- toList rawUsersStream
    close conn
    let users = mysqlToUsers rawUsers
    return users


createPhabricatorTickets :: [ManiphestTicket] -> IO ()
createPhabricatorTickets tickets =
    mapM_ createPhabricatorTicket tickets


badTickets = [8539]

createPhabricatorTicket :: ManiphestTicket -> IO (Either Text ())
createPhabricatorTicket ticket = do
    traceShowM (T.concat [T.pack (show $ m_tracn ticket),": ", m_title ticket])
    if m_tracn ticket `elem` badTickets
      then return (Left "BadTicket")
      else do
        response <- callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs ticket)
        case response of
            ConduitResult phidResponse -> do
              res <- postComment (api_phid phidResponse) ticket
            -- Make sure to do this last
              updatePhabricatorTicket (ticket {m_phid = Just (api_phid phidResponse)})
              return (Right ())
            ConduitError code info -> do
              traceShowM (m_title ticket, code, info)
              return $ Left (code `T.append` info)

buildTransactions :: ManiphestTicket -> [Value]
buildTransactions ManiphestTicket{m_changes} = mapMaybe doOne m_changes
  where
    doOne :: ManiphestChange -> Maybe Value
    doOne ManiphestChange{..} =
      case mc_type of
        MCComment c -> Just $ mkTransaction "comment" c
        MCCC cs -> Just $ mkTransaction "subscribers.set" cs
        MCArchitecture v -> Just $ mkTransaction "custom.ghc:architecture" v
        MCBlockedBy bs   -> Just $ mkTransaction "parent" bs
        MCComponent c    -> Just $ mkTransaction "custom.ghc:component" c
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
    , "ownerPHID" .= m_ownerPHID ticket
    , "priority" .= priorityToInteger (m_priority ticket)
    , "ccPHIDs" .= m_cc ticket
--    , "projectPHIDs" .= []  -- ["PHID-PROJ-qo3k34ztcwlndlg7pzkb" :: Text]
    ]

postComment :: TicketID -> ManiphestTicket -> IO ()
postComment phid mt = do
  let cs =  (m_changes mt)
  rawres <- callConduitPairs conduit "maniphest.edit"
            [ "objectIdentifier" .= phid
            , "transactions" .= buildTransactions mt ]
  let res = case rawres of
              ConduitResult r -> r
              _ -> error (show rawres)
  let ts = case J.parseEither transactionParser res of
        Left e -> error e
        Right r -> r
--  zipWithM_ (\c t -> fixCommentInformation t (mc_authorId c) (mc_created c)) cs ts
  return ()

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
fixCommentInformation :: TransactionID
                      -> UserID
                      -> DiffTime
                      -> IO ()
fixCommentInformation (PHID tid) (PHID maid) date =
  let fix1 = "UPDATE maniphest_transaction SET dateCreated=?, dateModified=?, authorPHID=? WHERE phid=?"
      fix2 = "UPDATE maniphest_transaction_comment SET authorPHID=? WHERE transactionPHID=?"
      values1 = [ MySQLInt64 $ convertTime date, MySQLInt64 $ convertTime date, MySQLText maid, MySQLText tid]
      values2 = [values1 !! 2, values1 !! 3]
  in do
    conn <- connect (phabConnectInfo { ciDatabase = "bitnami_phabricator_maniphest" })
    void $ execute conn fix1 values1 >> execute conn fix2 values2
    close conn




priorityToInteger :: ManiphestPriority -> Integer
priorityToInteger p =
    case p of
        Unbreak -> 100
        Triage -> 90
        High -> 80
        Normal -> 50
        Low -> 25
        Wishlist -> 0


updatePhabricatorTicket :: ManiphestTicket -> IO ()
updatePhabricatorTicket ticket = do
    conn <- connect (phabConnectInfo { ciDatabase = "bitnami_phabricator_maniphest" })
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
    close conn


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


convertTime :: DiffTime -> Int64
convertTime t = fromIntegral $ diffTimeToPicoseconds t `div` 1000000
