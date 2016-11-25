{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator where

import GHC.Generics
import Data.Int
import Data.Maybe (catMaybes, fromJust)
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
    conn <- connect defaultConnectInfo {ciDatabase = "phabricator_user", ciPassword = "foobar", ciPort = 32773}
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
    let authToken = ConduitAPITokenAuth "api-gptwkv5kg4nayou7gl5zasm3u5hu"
    let conduit = Conduit "http://phabricator.dev/api" authToken
    response <- callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs ticket)
    return $ case response of
        ConduitResult phidResponse -> Right ticket {m_phid = Just (api_phid phidResponse)}
        ConduitError code info -> Left (code `T.append` info)


ticketToConduitPairs :: ManiphestTicket -> [J.Pair]
ticketToConduitPairs ticket =
    [ "title" .= (m_title ticket)
    , "description"  .= (m_description ticket)
    , "ownerPHID" .= (m_ownerPHID ticket)
    , "priority" .= (priorityToInteger $ m_priority ticket)
    , "projectPHIDs" .= ["PHID-PROJ-qo3k34ztcwlndlg7pzkb" :: Text]
    ]


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
    conn <- connect defaultConnectInfo {ciDatabase = "phabricator_maniphest", ciPassword = "foobar", ciPort = 32773}
    let q = "UPDATE maniphest_task SET dateCreated=?, dateModified=?, status=? WHERE phid=?;"
    forM_ tickets $ \ticket -> do
        case ticketToUpdateTuple ticket of
            Just values -> void $ execute conn q values
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
