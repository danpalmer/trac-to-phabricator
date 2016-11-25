{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trac where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (liftM, liftM2)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Maybe
import Config

data TracTicket = TracTicket
    { t_id :: Int
    , t_type :: Text
    , t_time :: DiffTime
    , t_changetime :: DiffTime
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
        <*> fmap tracTimeToDiffTime field
        <*> fmap tracTimeToDiffTime field
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
        <*> liftM2 TracCustomField field field

data TracTicketChange = TracTicketChange
    { ch_ticket :: Int
    , ch_time :: DiffTime
    , ch_author :: Text
    , ch_field :: Text
    , ch_oldvalue :: Maybe Text
    , ch_newvalue :: Maybe Text
    }

{-
 _comment15
 failure
 type
 status
 _comment14
 milestone
 reporter
 description
 patch
 wikipage
 severity
 _comment2
 architecture
 _comment6
 _comment9
 _comment0
 differential
 _comment5
 _comment10
 comment
 _comment4
 related
 _comment16
 cc
 summary
 _comment7
 _comment11
 testcase
 difficulty
 keywords
 resolution
 component
 _comment12
 owner
 priority
 _comment8
 blockedby
 _comment1
 _comment13
 blocking
 os
 version
 _comment3
-}

instance FromRow TracTicketChange where
    fromRow = TracTicketChange
        <$> field
        <*> fmap tracTimeToDiffTime field
        <*> field
        <*> field
        <*> field
        <*> field

data TracTicketComment = TracTicketComment
    { co_ticket :: Int
    , co_time :: DiffTime
    , co_author :: Text
    , co_comment :: Text
    } deriving (Generic, Show)

instance FromRow TracTicketComment where
    fromRow = TracTicketComment
        <$> field
        <*> fmap tracTimeToDiffTime field
        <*> field
        <*  (field :: RowParser Text)
        <*  (field :: RowParser Text)
        <*> field


tracTimeToDiffTime :: Integer -> DiffTime
tracTimeToDiffTime = picosecondsToDiffTime


mergeTracData :: [TracTicket] -> [TracCustomFieldRelation] -> [TracTicketChange] -> [TracTicket]
mergeTracData tickets fields comments
  = mergeTicketsAndComments (mergeTicketsAndCustomFields tickets fields) comments


mergeTicketsAndCustomFields :: [TracTicket] -> [TracCustomFieldRelation] -> [TracTicket]
mergeTicketsAndCustomFields tickets customFields = fmap merge tickets
    where
        merge ticket = ticket {t_customFields = cfr_customField <$> fields ticket}
        fields ticket = filter (\x -> t_id ticket == cfr_ticket x) customFields

-- Very expensive currently
mergeTicketsAndComments :: [TracTicket] -> [TracTicketChange] -> [TracTicket]
mergeTicketsAndComments tickets changes = map merge tickets
    where
        comments = mapMaybe getComments changes
        merge ticket = ticket {t_comments = ticketComments ticket}
        ticketComments ticket = filter (\x -> t_id ticket == co_ticket x) comments

-- Going to change this into a more general method which maps a change to
-- a maniphest update
getComments :: TracTicketChange -> Maybe TracTicketComment
getComments TracTicketChange{..} =
  -- If a user makes a change to the ticket, an empty comment is also added
  -- to the changes
	if ch_field == "comment" && not (T.null (fromMaybe "" ch_newvalue))
		then Just $ TracTicketComment ch_ticket ch_time ch_author (fromJust ch_newvalue)
		else Nothing


getTracTickets :: IO [TracTicket]
getTracTickets = do
    conn <- connect tracConnectInfo
    rawTickets <- query_ conn "SELECT * FROM ticket"
    customFields <- query_ conn "SELECT * FROM ticket_custom"
    ticketUpdates <- query_ conn "SELECT * FROM ticket_change"
    close conn
    return $ mergeTracData rawTickets customFields ticketUpdates
