{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Trac where

import GHC.Generics
import Data.Text (Text)
import Control.Monad (liftM, liftM2)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

data TracTicket = TracTicket
    { t_id :: Int
    , t_type :: Text
    , t_time :: UTCTime
    , t_changetime :: UTCTime
    , t_component :: Text
    , t_severity :: Maybe Text
    , t_priority :: Text
    , t_owner :: Maybe Text
    , t_reporter :: Text
    , t_cc :: Maybe Text
    , t_version :: Maybe Text
    , t_milestone :: Maybe Text
    , t_status :: Text
    , t_resolution :: Maybe Text
    , t_summary :: Text
    , t_description :: Maybe Text
    , t_keywords :: Maybe Text
    , t_customFields :: [TracCustomField]
    , t_comments :: [TracTicketComment]
    } deriving (Generic, Show)

instance FromRow TracTicket where
    fromRow = TracTicket
        <$> field
        <*> field
        <*> (liftM tracTimeToUTCTime field)
        <*> (liftM tracTimeToUTCTime field)
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> pure []
        <*> pure []

data TracCustomField = TracCustomField
    { cf_name :: Text
    , cf_value :: Maybe Text
    } deriving (Generic, Show)

data TracCustomFieldRelation = TracCustomFieldRelation
    { cfr_ticket :: Int
    , cfr_customField :: TracCustomField
    } deriving (Generic, Show)

instance FromRow TracCustomFieldRelation where
    fromRow = TracCustomFieldRelation
        <$> field
        <*> (liftM2 TracCustomField field field)

data TracTicketChange = TracTicketChange
    { ch_ticket :: Int
    , ch_time :: UTCTime
    , ch_author :: Text
    , ch_field :: Text
    , ch_oldvalue :: Text
    , ch_newvalue :: Text
    }

instance FromRow TracTicketChange where
    fromRow = TracTicketChange
        <$> field
        <*> (liftM tracTimeToUTCTime field)
        <*> field
        <*> field
        <*> field
        <*> field


data TracTicketComment = TracTicketComment
    { co_ticket :: Int
    , co_time :: UTCTime
    , co_author :: Text
    , co_comment :: Text
    } deriving (Generic, Show)

instance FromRow TracTicketComment where
    fromRow = TracTicketComment
        <$> field
        <*> (liftM tracTimeToUTCTime field)
        <*> field
        <*  (field :: RowParser Text)
        <*  (field :: RowParser Text)
        <*> field


tracTimeToUTCTime :: Integer -> UTCTime
tracTimeToUTCTime = posixSecondsToUTCTime . toNominalDiffTime . picosecondsToDiffTime
    where toNominalDiffTime = fromRational . toRational


mergeTracData :: [TracTicket] -> [TracCustomFieldRelation] -> [TracTicketComment] -> [TracTicket]
mergeTracData tickets fields comments = mergeTicketsAndComments (mergeTicketsAndCustomFields tickets fields) comments


mergeTicketsAndCustomFields :: [TracTicket] -> [TracCustomFieldRelation] -> [TracTicket]
mergeTicketsAndCustomFields tickets customFields = fmap merge tickets
    where
        merge ticket = ticket {t_customFields = cfr_customField <$> (fields ticket)}
        fields ticket = filter (\x -> t_id ticket == cfr_ticket x) customFields

mergeTicketsAndComments :: [TracTicket] -> [TracTicketComment] -> [TracTicket]
mergeTicketsAndComments tickets comments = fmap merge tickets
    where
        merge ticket = ticket {t_comments = (ticketComments ticket)}
        ticketComments ticket = filter (\x -> t_id ticket == co_ticket x) comments


getTracTickets :: IO ([TracTicket])
getTracTickets = do
    conn <- connect defaultConnectInfo {connectDatabase = "ghc_trac"}
    rawTickets <- query_ conn "SELECT * FROM ticket"
    customFields <- query_ conn "SELECT * FROM ticket_custom"
    ticketComments <- query_ conn "SELECT * FROM ticket_change WHERE field = 'comment'"
    close conn
    return $ mergeTracData rawTickets customFields ticketComments
