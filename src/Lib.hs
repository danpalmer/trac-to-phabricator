
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}


module Lib
    ( migrate
    , describeTicket
    , WorkDescription(.., DownTo)
    ) where

import qualified Data.Text as T
import Data.List (find)

import Trac
import Phabricator
import Config
import Types
import Debug.Trace
import Data.Maybe
import Data.List
import Data.Ord
import Control.Applicative

import qualified Database.MySQL.Base as M
import qualified Database.PostgreSQL.Simple as P

data WorkDescription = Exact Int | UpTo Bool Int | All

migrate :: WorkDescription -> IO ()
migrate workDesc = do
    tracConn <- P.connect tracConnectInfo
    pc@PC{..} <- connectPhab
    phabricatorUsers <- getPhabricatorUsers pcUser
    traceShowM ("phabUsers", length phabricatorUsers)
    tracTickets <- getTracTickets tracConn
    traceShowM ("tickets", length tracTickets)
    let tracTickets' = getTickets workDesc (sortBy (comparing t_id) tracTickets)
    let phabricatorTickets = map (tracTicketToPhabricatorTicket phabricatorUsers) tracTickets'
    createPhabricatorTickets pcManiphest phabricatorTickets
    P.close tracConn
    closePhab pc
    putStrLn $ "Migrated " ++ show (length tracTickets') ++ " tickets."


pattern DownTo n = UpTo True n


getTickets :: WorkDescription -> [TracTicket] -> [TracTicket]
getTickets (Exact n) ts = maybe [] (:[])  (find ((== n) . t_id) ts)
getTickets (UpTo rev n) ts  = take n (if rev then reverse ts else ts)
getTickets All ts = ts


describeTicket :: TracTicket -> String
describeTicket ticket = T.unpack $
    T.concat [
        t_summary ticket,
            "\n\tFields: ", textLength $ t_customFields ticket,
            "\n\tComments:", textLength $ t_comments ticket,
            "\n\n"
    ]
    where textLength = T.pack . show . length


tracTicketToPhabricatorTicket :: [PhabricatorUser] -> TracTicket -> ManiphestTicket
tracTicketToPhabricatorTicket users ticket =
    ManiphestTicket
        { m_tracn = t_id ticket
        , m_title = t_summary ticket
        , m_description = convert <$> t_description ticket
        , m_ownerPHID = findUser <$> t_owner ticket
        , m_authorPHID = findUser $ t_reporter ticket
        , m_priority = convertPriority $ t_priority ticket
        , m_created = t_time ticket
        , m_modified = t_changetime ticket
        , m_phid = Nothing
        , m_status = t_status ticket
        , m_changes = map (tracChangeToPhabChange users) (t_comments ticket)
        , m_cc = mapMaybe lookupCC (t_cc ticket)
        }
    where findUser u = fromMaybe botUser (lookupPhabricatorUserPHID users u)
          -- CC field is either an email or a username
          lookupCC t = lookupPhabricatorUserPHID users t <|> lookupByEmail t

-- I don't have mails to check now
lookupByEmail = const Nothing

tracChangeToPhabChange :: [PhabricatorUser] -> TracTicketChange -> ManiphestChange
tracChangeToPhabChange users TracTicketChange{..}
  = ManiphestChange
      { mc_type    = trace (take 50 $ show (getType ch_field)) (getType ch_field)
      , mc_created = ch_time
      , mc_authorId = findUser ch_author }
  where
    findUser u = fromMaybe botUser (lookupPhabricatorUserPHID users u)
    getType :: T.Text -> MCType
    getType t =
      case t of
        "comment" -> MCComment (convert $ fromMaybe "" ch_newvalue)
        "cc"      -> MCCC (maybe [] (\v -> mapMaybe lookupCC (parse_cc v)) ch_newvalue)
        "architecture" -> maybe Dummy MCArchitecture ch_newvalue
        "blockedby" -> MCBlockedBy []
--        "blocking"  -> MCBlocking []
        "component" -> m MCComponent
        "description" -> m MCDescription
        "differential" -> MCDifferential []
        "difficulty"   -> m MCDifficulty
        "failure"      -> m MCFailure
        "keywords"     -> MCKeywords []
        "milestone"    -> m MCMilestone
        "os"           -> m MCOS
        "owner"        -> maybe Dummy MCOwner (findUser <$> ch_newvalue)
        "patch"        -> MCPatch
        "priority"     -> MCPriority (maybe Normal convertPriority ch_newvalue)
        "related"      -> MCRelated
        "reporter"     -> MCReporter
        "resolution"   -> MCResolution
        "severity"     -> MCSeverity
        "status"       -> m MCStatus
        "summary"      -> m MCSummary
        "testcase"     -> MCTestcase
        "type"         -> m MCType
        "version"      -> m MCVersion
        "wikipage"     -> m MCWiki
        _         -> Dummy
        -- Note there are lots of entries like _comment1 which correspond
        -- to comment updates. However, the value in comment is the actual
        -- final comment and we don't both to maintain this much fidelity.

    m con = maybe Dummy con ch_newvalue
    lookupCC t = lookupPhabricatorUserPHID users t <|> lookupByEmail t


convertPriority :: T.Text -> ManiphestPriority
convertPriority priority =
    case priority of
        "highest" -> Unbreak
        "high" -> High
        "normal" -> Normal
        "low" -> Low
        "lowest" -> Wishlist
        _ -> Triage


lookupPhabricatorUserPHID :: [PhabricatorUser] -> T.Text -> Maybe UserID
lookupPhabricatorUserPHID users username  =
    u_phid <$> find (\x -> u_userName x == username) users
