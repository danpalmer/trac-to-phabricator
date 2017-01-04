{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.IntMap (IntMap)
import Data.Ord
import Data.List
import qualified Data.IntMap as M
import Debug.Trace
import Util
import Network.URI.Encode
import Control.Monad
import Data.Char

connectTrac :: IO Connection
connectTrac = connect tracConnectInfo

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
    , t_keywords :: [Text]
    , t_customFields :: [TracCustomField]
    , t_changes :: [TracTicketChange]
    , t_diffs :: [Int]   -- Initial Diffs
    , t_commits :: [TCommit]
    , t_attachments :: [Attachment]
    , t_related :: [Int] -- Initial Related Tickets
    , t_blockedby :: [Int]
    , t_blocking :: [Int]
    , t_os :: Maybe Text
    , t_architecture :: Maybe Text
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
        <*> (maybe [] parse_cc <$> (field :: RowParser (Maybe Text)))
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure []
        <*> pure Nothing
        <*> pure Nothing

data TCommit = TCommit {
              c_id :: Text
              , c_author :: Text
              , c_time :: DiffTime
              } deriving Show

parseList :: Text -> [Text]
parseList = map T.strip . T.splitOn ","

parse_cc :: Text -> [Text]
parse_cc = parseList

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
        merge ticket = ticket {t_changes = ticketComments ticket}
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
    am <- getAttachmentMap conn
    return $
        map (normalise . recoverOriginalTracTicket am) $
          mergeTracData rawTickets customFields ticketUpdates


-- We want to recover the original state of the ticket.
recoverOriginalTracTicket :: AttachmentMap -> TracTicket -> TracTicket
recoverOriginalTracTicket am TracTicket{..} =
  TracTicket
    { t_id = t_id
    , t_type = recover "type" t_changes t_type
    , t_time = t_time
    , t_changetime = t_changetime
    , t_component = recover "component" t_changes t_component
    , t_severity = recover "severity" t_changes <$> t_severity
    , t_priority = recover "priority" t_changes t_priority
    , t_owner = recoverG "owner" t_changes id t_owner
    , t_reporter = t_reporter
    , t_cc = recoverG "cc" t_changes n t_cc
    , t_version = recoverG "version" t_changes id t_version
    , t_milestone = recoverG "milestone" t_changes id t_milestone
    , t_status = recover "status" t_changes t_status
    , t_resolution = recoverG "resolution" t_changes id t_resolution
    , t_summary = t_summary
    , t_description = t_description
    , t_keywords = recoverG "keywords" t_changes n t_keywords
    , t_customFields = t_customFields
    , t_changes = tChanges
    , t_diffs = recoverG "differential" t_changes parseDiff
                  (parseDiff $ recoverCustomFieldCurrent
                                          "differential"
                                          t_customFields)
    , t_commits = tCommits
    , t_attachments = fromMaybe [] (M.lookup t_id am)
    , t_related = recoverG "related" t_changes parseTicketName
                    (parseTicketName $ recoverCustomFieldCurrent
                                        "related"
                                        t_customFields)
    , t_blockedby = recoverTList "blockedby"
    , t_blocking = recoverTList "blocking"
    , t_os =
        recoverG "os" t_changes id
         $ recoverCustomFieldCurrent  "os" t_customFields
    , t_architecture =
        recoverG "architecture" t_changes id
         $ recoverCustomFieldCurrent  "architecture" t_customFields
    }
  where
    recoverG :: Text -> [TracTicketChange] -> (Maybe Text -> a) -> a -> a
    recoverG field cs f def = maybe def f (ch_oldvalue <$> find ((==field) . ch_field) cs)

    recover :: Text -> [TracTicketChange] -> Text -> Text
    recover field cs def = recoverG field cs fromJust def

    recoverTList field = recoverG field t_changes parseTList
                          (parseTList $
                            recoverCustomFieldCurrent field t_customFields)


    n :: Maybe Text -> [Text]
    n = maybe [] parse_cc



    recoverCustomFieldCurrent :: Text -> [TracCustomField] -> Maybe Text
    recoverCustomFieldCurrent t tcs =
      join (cf_value <$> find ((== t) . cf_name) tcs)

    -- Commit comments have spaces in author names
    isCommitComment TracTicketChange { ch_field = "comment", ch_author = a
                                     , ch_newvalue = Just nv }
      =  (isJust $ T.find (== '@') a)
          && (start2 `T.isPrefixOf` nv
          || (start1 `T.isPrefixOf` nv))

    isCommitComment _ = False

    (tCommitsRaw, tChanges) = partition isCommitComment t_changes

    tCommits = map getCommit tCommitsRaw

    getCommit :: TracTicketChange -> TCommit
    getCommit TracTicketChange{..} =
      TCommit { c_id = getCommitHash (fromJust ch_newvalue)
             , c_author = ch_author
             , c_time = ch_time }

    start = "In [changeset:\""
    start1 = "commit "
    start2 = "In [changeset:" -- Note without ":"

    getCommitHash s =
     if start `T.isPrefixOf` s
        then T.takeWhile (/= '/') (T.drop (T.length start) s)
        else if start2 `T.isPrefixOf` s
               then T.takeWhile (/= '/') (T.drop (T.length start2) s)
               else if start1 `T.isPrefixOf` s
                  then T.takeWhile (/= '\n') (T.drop (T.length start1) s)
                  else error (show s)

parseTList :: Maybe Text -> [Int]
parseTList = maybe [] (recov . T.unpack)
      where
        recov :: String -> [Int]
        recov [] = []
        recov (c:cs)
          | isDigit c = let (ds, res) = span isDigit cs in read (c:ds)
                                                            : recov res
          | otherwise = recov cs

parseTicketName :: Maybe Text -> [Int]
parseTicketName =  parseLeaderGen '#'

parseDiff :: Maybe Text -> [Int]
parseDiff = parseLeaderGen 'D'

parseLeaderGen :: Char -> Maybe Text -> [Int]
parseLeaderGen l = maybe [] (find_gen False . T.unpack)
  where
    find_gen :: Bool -> String -> [Int]
    find_gen _ [] = []
    find_gen False (c:s) = find_gen (c == l) s
    find_gen True s =
          let (n, rest) = span isDigit (dropWhile (not . isDigit) s)
          in read n : find_gen False rest


normalise :: TracTicket -> TracTicket
normalise t = t { t_cc = filter (not . T.null) (t_cc t)
                , t_keywords = filter (not . T.null) (t_keywords t)
                , t_milestone = flatten t_milestone
                , t_owner = flatten t_owner
                , t_resolution = flatten t_resolution
                , t_changes = insertResolution (t_changes t) }
  where
    flatten f = case f t of
                  Just "" -> Nothing
                  Just "nobody" -> Nothing
                  v -> v

    mkFakeResolution c = c { ch_field="resolution", ch_newvalue= Just "fixed" }

    -- Early tickets are closed without a resolution which breaks things
    -- later
    insertResolution [] = []
    insertResolution (r@Resolution:c@Closed:xs) = r:c: insertResolution xs
    insertResolution (c@Closed:xs) = mkFakeResolution c : c : insertResolution xs
    insertResolution (x:xs) = x : insertResolution xs


pattern Closed :: TracTicketChange
pattern Closed <- TracTicketChange { ch_field = "status", ch_newvalue = Just "closed" }

pattern Resolution :: TracTicketChange
pattern Resolution <- TracTicketChange { ch_field = "resolution"}




type TracUser = Text

getTracUsers :: Connection -> IO [Text]
getTracUsers conn = do
  map fromOnly <$> query_ conn "SELECT DISTINCT author FROM ticket_change"

data Projects = Projects
              { keywords :: [Text] -- Make these into projects
              , oses :: [Text]     -- These into subprojects
              , arch :: [Text]
              , milestones :: [Text] -- Milestones of "GHC"
              , comp :: [Text]
              , types :: [Text] } -- Projects }
              deriving Show


-- This is for keywords and also custom more structured fields
getProjectWords :: Connection -> IO Projects
getProjectWords conn = do
  tags <- processKeywords . mapMaybe fromOnly <$> query_ conn "SELECT keywords FROM ticket"
  os <- delete "Other" . removeDefault "Unknown/Multiple" . map fromOnly
            <$> query_ conn "SELECT DISTINCT value FROM ticket_custom WHERE name='os'"
  archs <- delete "Other" . removeDefault "Unknown/Multiple" . map fromOnly
            <$> query_ conn "SELECT DISTINCT value FROM ticket_custom WHERE name='architecture'"
  milestone <- delete "None" . map fromOnly <$> query_ conn "SELECT name FROM milestone"
  component <- removeDefault "Compiler" . map fromOnly <$> query_ conn "SELECT name FROM component"
  let types = ["task", "feature request", "bug"]
  return $ Projects tags os archs milestone component types
  where
    removeDefault = delete

-- Remove spaces and other bad characters
normaliseToProjectName :: Text -> Text
normaliseToProjectName t = T.replace " " "-" t



-- We only pick keywords with at least 10 tickets, seems like a good time
-- for a cleanup!
processKeywords :: [Text] -> [Text]
processKeywords ts =
  let all = concatMap parse_cc ts
      uni = nub all

      count x = length . filter (== x)
      counts = sortBy (comparing snd) (map (\v -> (v, count v all)) uni)
      final = map fst (filter (\(_, n) -> n > 5) counts)
  in traceShow counts final

-- Downloading Attachments

data Attachment = Attachment {
                     a_type :: Text
                   , a_id :: Int
                   , a_name :: Text
                   , a_size :: Int
                   , a_time :: DiffTime
                   , a_desc :: Text
                   , a_author :: Text
                   , a_ip :: Maybe Text
                    } deriving Show

attachmentToPath :: Attachment -> Text
attachmentToPath Attachment{a_id, a_name}
  = mkAttachmentPath a_id a_name

mkAttachmentPath :: Int -> Text -> Text
mkAttachmentPath a_id a_name
  = T.concat ["attachments/", T.pack (show a_id),"-", a_name]

instance FromRow Attachment where
  fromRow = Attachment
        <$> field -- Type
        <*> ((read @Int) . T.unpack  <$> field) -- ID
        <*> field
        <*> field
        <*> fmap tracTimeToDiffTime field
        <*> field
        <*> field
        <*>  field

getAttachments :: Connection -> IO [Attachment]
getAttachments conn = query_ conn "SELECT * FROM attachment WHERE type='ticket'"

getAttachmentMap :: Connection -> IO AttachmentMap
getAttachmentMap conn = do
  as <- getAttachments conn
  return $ foldr (\a -> M.insertWith (++) (a_id a) [a]) mempty as

type AttachmentMap = IntMap [Attachment]

tracAttachmentURL :: Attachment -> Text
tracAttachmentURL Attachment{a_id, a_name}
  = T.concat ["https://ghc.haskell.org/trac/ghc/raw-attachment/ticket/"
             , T.pack (show a_id), "/", encodeText a_name]

downloadAttachments :: [Attachment] -> IO ()
downloadAttachments as =
  mapM_ doOne as
  where
    doOne :: Attachment -> IO ()
    doOne a =
      downloadFile (tracAttachmentURL a) (attachmentToPath a)


