{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Phabricator where

import GHC.Generics
import Data.Int
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Either (lefts, rights)
import Data.Monoid ((<>))
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

import qualified Trac.Convert as T

convert = T.pack . T.convert . T.unpack

data ManiphestTicketID = ManiphestTicketID
type ManiphestTicketPHID = PHID ManiphestTicketID

data APIPHID = APIPHID
    { api_phid :: ManiphestTicketPHID
    } deriving (Show, Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''APIPHID)

data ManiphestPriority = Unbreak | Triage | High | Normal | Low | Wishlist
    deriving (Show)

data PhabricatorUser = PhabricatorUser
    { u_phid         :: PHID PhabricatorUser
    , u_userName     :: Text
    } deriving (Show)

type UserID = PHID PhabricatorUser

data ManiphestTicket = ManiphestTicket
    { m_title :: Text
    , m_description :: Maybe Text
    , m_authorPHID :: Maybe UserID
    , m_ownerPHID :: Maybe UserID
    , m_priority :: ManiphestPriority
    , m_created :: DiffTime
    , m_modified :: DiffTime
    , m_phid :: Maybe ManiphestTicketPHID
    , m_status :: Text
		, m_changes :: [Comment]
    } deriving (Show)

type Comment = TracTicketComment


mysqlToUser :: [MySQLValue] -> Maybe PhabricatorUser
mysqlToUser values =
    case values of
        [x,y] -> PhabricatorUser <$> (decodePHID x) <*> (decodeUserName y)
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
    close conn
    rawUsers <- toList rawUsersStream
    let users = mysqlToUsers rawUsers
    print users
    return []


createPhabricatorTickets :: [ManiphestTicket] -> IO [ManiphestTicket]
createPhabricatorTickets tickets = do
    tickets' <- mapM createPhabricatorTicket tickets
    mapM_ (putStrLn . T.unpack) $ lefts tickets'
    return $ rights tickets'



createPhabricatorTicket :: ManiphestTicket -> IO (Either Text ManiphestTicket)
createPhabricatorTicket ticket = do
    response <- callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs ticket)
    case response of
        ConduitResult phidResponse -> do
          res <- postComment (api_phid phidResponse) ticket
          traceShowM res
          return $ Right ticket {m_phid = Just (api_phid phidResponse)}
        ConduitError code info -> return $ Left (code `T.append` info)

buildTransactions :: ManiphestTicket -> [Value]
buildTransactions ManiphestTicket{m_changes} = map doOne (traceShowId m_changes)
	where
		doOne :: Comment -> Value
		doOne (TracTicketComment{..}) = object ["type" .= ("comment" :: Text) , "value" .= convert (co_comment)]


ticketToConduitPairs :: ManiphestTicket -> [J.Pair]
ticketToConduitPairs ticket =
    [ "title" .= (m_title ticket)
    , "description"  .= (m_description ticket)
    , "ownerPHID" .= (m_ownerPHID ticket)
    , "priority" .= (priorityToInteger $ m_priority ticket)
--    , "projectPHIDs" .= []  -- ["PHID-PROJ-qo3k34ztcwlndlg7pzkb" :: Text]
    ]

postComment :: ManiphestTicketPHID -> ManiphestTicket -> IO (ConduitResponse Object)
postComment phid mt = do
  res <- callConduitPairs conduit "maniphest.edit" [ "objectIdentifier" .= phid , "transactions" .= buildTransactions mt ]
  return res



priorityToInteger :: ManiphestPriority -> Integer
priorityToInteger p =
    case p of
        Unbreak -> 100
        Triage -> 90
        High -> 80
        Normal -> 50
        Low -> 25
        Wishlist -> 0


updatePhabricatorTickets :: [ManiphestTicket] -> IO ()
updatePhabricatorTickets tickets = do
    conn <- connect (phabConnectInfo { ciDatabase = "bitnami_phabricator_maniphest" })
    let q = "UPDATE maniphest_task SET dateCreated=?, dateModified=?, status=? WHERE phid=?;"
        q2 = "UPDATE maniphest_transaction SET dateCreated=? WHERE objectPHID=?" -- Some queries go from this separate table rather than the actual information in the ticket.
    forM_ tickets $ \ticket ->
        case ticketToUpdateTuple ticket of
            Just values -> void $ execute conn q values >> execute conn q2 [values !! 0, values !! 3]
            Nothing -> return ()
    close conn
    return ()


ticketToUpdateTuple :: ManiphestTicket -> Maybe [MySQLValue]
ticketToUpdateTuple ticket =
    case (m_phid ticket) of
        Just (PHID t) -> Just
            [ MySQLInt64 $ convertTime (m_created ticket)
            , MySQLInt64 $ convertTime (m_modified ticket)
            , MySQLText $ m_status ticket
            , MySQLText t
            ]
        Nothing -> Nothing


convertTime :: DiffTime -> Int64
convertTime t = fromIntegral $ (diffTimeToPicoseconds t) `div` 1000000
