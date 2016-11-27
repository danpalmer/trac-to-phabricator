{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Trac where

import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (liftM2)
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Maybe
import Config
import Data.IntMap (IntMap, (!))
import Data.Ord
import Data.List
import qualified Data.IntMap as M
import qualified Data.Set as S
import Debug.Trace

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
    , t_cc :: [Text]
    , t_version :: Maybe Text
    , t_milestone :: Maybe Text
    , t_status :: Text
    , t_resolution :: Maybe Text
    , t_summary :: Text
    , t_description :: Maybe Text
    , t_keywords :: Maybe Text
    , t_customFields :: [TracCustomField]
    , t_comments :: [TracTicketChange]
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
        <*> (maybe [] parse_cc <$> (field :: RowParser (Maybe Text)))
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> pure []
        <*> pure []

parse_cc :: Text -> [Text]
parse_cc = map T.strip . T.splitOn ","

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
    } deriving Show

{-
 _comment0
 _comment1
 _comment10
 _comment11
 _comment12
 _comment13
 _comment14
 _comment15
 _comment16
 _comment2
 _comment3
 _comment4
 _comment5
 _comment6
 _comment7
 _comment8
 _comment9
 architecture
 blockedby
 blocking
 cc
 comment
 component
 description
 differential
 difficulty
 failure
 keywords
 milestone
 os
 owner
 patch
 priority
 related
 reporter
 resolution
 severity
 status
 summary
 testcase
 type
 version
 wikipage
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

type DList a = [a] -> [a]

emptyD :: [a] -> [a]
emptyD = id

singletonD :: a -> [a] -> [a]
singletonD = (:)

concatD :: DList a ->  DList a -> DList a
concatD = (.)

mergeTicketsAndCustomFields :: [TracTicket] -> [TracCustomFieldRelation] -> [TracTicket]
mergeTicketsAndCustomFields tickets customFields = fmap merge tickets
    where
        bucket :: IntMap (DList TracCustomField)
        bucket = M.fromAscListWith concatD [(cfr_ticket x, singletonD (cfr_customField x)) | x <- customFields]
        merge ticket = ticket {t_customFields = fields ticket }
        fields ticket = M.findWithDefault id (t_id ticket) bucket []

mergeTicketsAndComments :: [TracTicket] -> [TracTicketChange] -> [TracTicket]
mergeTicketsAndComments tickets changes = map merge tickets
    where
        changes' = filter getComments changes
        bucket :: IntMap (DList TracTicketChange)
        bucket = M.fromAscListWith concatD [(ch_ticket x, singletonD x) | x <- changes']
        merge ticket = ticket {t_comments = ticketComments ticket}
        ticketComments ticket =
          sortBy (comparing ch_time)
           $ M.findWithDefault id (t_id ticket) bucket []

-- Going to change this into a more general method which maps a change to
-- a maniphest update
getComments :: TracTicketChange -> Bool
getComments TracTicketChange{..} =
  -- If a user makes a change to the ticket, an empty comment is also added
  -- to the changes
  if ch_field == "comment"
    then maybe False (not . T.null) ch_newvalue
    else True

getTracTickets :: Connection -> IO [TracTicket]
getTracTickets conn = do
    rawTickets <- query_ conn "SELECT * FROM ticket"
    customFields <- query_ conn "SELECT * FROM ticket_custom ORDER BY ticket"
    ticketUpdates <- query_ conn "SELECT * FROM ticket_change ORDER BY ticket"
    return $ mergeTracData rawTickets customFields ticketUpdates

type TracUser = Text

getTracUsers :: Connection -> IO [Text]
getTracUsers conn = do
  map fromOnly <$> query_ conn "SELECT DISTINCT author FROM ticket_change"


data KeywordType = Keyword deriving Show

-- This is for keywords and also custom more structured fields
getProjectWords :: Connection -> IO [(KeywordType, Text)]
getProjectWords conn = do
  tags <- processKeywords . mapMaybe fromOnly <$> query_ conn "SELECT keywords FROM ticket"
  return (map (Keyword,) tags)

-- We only pick keywords with at least 10 tickets, seems like a good time
-- for a cleanup!
processKeywords :: [Text] -> [Text]
processKeywords ts =
  let all = concatMap parse_cc ts
      uni = nub all

      count x = length . filter (== x)
      counts = sortBy (comparing snd) (map (\v -> (v, count v all)) uni)
      final = map fst (filter (\(v, n) -> n > 10) counts)
  in traceShow counts final


