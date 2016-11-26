{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Trac.Convert as T

convert :: Text -> Text
convert = T.pack . T.convert . T.unpack

-- Used as a kind
data PHIDType = Ticket | Author | Transaction

type ManiphestTicketPHID = PHID 'Ticket
type ManiphestTransactionPHID = PHID 'Transaction
type ManiphestAuthorPHID = PHID 'Author

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
    rawUsers <- toList rawUsersStream
    close conn
    let users = mysqlToUsers rawUsers
    return users


createPhabricatorTickets :: [ManiphestTicket] -> IO ()
createPhabricatorTickets tickets = do
    tickets' <- mapM createPhabricatorTicket tickets
    mapM_ (putStrLn . T.unpack) $ lefts tickets'



createPhabricatorTicket :: ManiphestTicket -> IO (Either Text ())
createPhabricatorTicket ticket = do
    response <- callConduitPairs conduit "maniphest.createtask" (ticketToConduitPairs ticket)
    case response of
        ConduitResult phidResponse -> do
          res <- postComment (api_phid phidResponse) ticket
          updatePhabricatorTicket (ticket {m_phid = Just (api_phid phidResponse)})
          return (Right ())
        ConduitError code info -> return $ Left (code `T.append` info)

buildTransactions :: ManiphestTicket -> [Value]
buildTransactions ManiphestTicket{m_changes} = map doOne (traceShowId m_changes)
  where
    doOne :: Comment -> Value
    doOne (TracTicketComment{..}) = object ["type" .= ("comment" :: Text)
                                           , "value" .= convert (co_comment)]


ticketToConduitPairs :: ManiphestTicket -> [J.Pair]
ticketToConduitPairs ticket =
    [ "title" .= (m_title ticket)
    , "description"  .= (m_description ticket)
    , "ownerPHID" .= (m_ownerPHID ticket)
    , "priority" .= (priorityToInteger $ m_priority ticket)
--    , "projectPHIDs" .= []  -- ["PHID-PROJ-qo3k34ztcwlndlg7pzkb" :: Text]
    ]

postComment :: ManiphestTicketPHID -> ManiphestTicket -> IO ()
postComment phid mt = do
  let cs = m_changes mt
  (ConduitResult res :: ConduitResponse Object) <- callConduitPairs conduit "maniphest.edit"
            [ "objectIdentifier" .= phid
            , "transactions" .= buildTransactions mt ]
  let ts = case J.parseEither transactionParser res of
        Left e -> error e
        Right r -> r
  let   slyfox :: ManiphestAuthorPHID
        slyfox = PHID "PHID-USER-r77ofkse6266v7ngjzlk"
  zipWithM_ (\c t -> fixCommentInformation t slyfox (co_time c)) cs ts
  return ()

transactionParser :: Object -> J.Parser [ManiphestTransactionPHID]
transactionParser o = do
  traceShowM o
  ts <- o .: "transactions"
  J.listParser (withObject "" (\v -> v .: "phid")) ts
--  parseJSON ts



-- Need to go into two tables,  phabricator_manifest_transaction and
-- phabricator_manifest_comment
fixCommentInformation :: ManiphestTransactionPHID
                      -> ManiphestAuthorPHID
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
    let q = "UPDATE maniphest_task SET dateCreated=?, dateModified=?, status=? WHERE phid=?;"
        -- Some queries go from this separate table rather than the actual information in the ticket.
        q2 = "UPDATE maniphest_transaction SET dateCreated=? WHERE objectPHID=?"
    case ticketToUpdateTuple ticket of
      Just values -> void $ execute conn q values >> execute conn q2 [values !! 0, values !! 3]
      Nothing -> return ()
    close conn


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
