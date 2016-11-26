{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module Lib
    ( migrate
    , describeTicket
    ) where

import qualified Data.Text as T
import Data.List (find)

import Trac
import Phabricator
import Config
import Types
import Debug.Trace
import Data.Maybe

migrate :: IO ()
migrate = do
    phabricatorUsers <- getPhabricatorUsers
    traceShowM ("phabUsers", length phabricatorUsers)
    tracTickets <- getTracTickets
    traceShowM ("tickets", length $ tracTickets)
    let tracTickets' = take 3 (reverse tracTickets)
    let phabricatorTickets = map (tracTicketToPhabricatorTicket phabricatorUsers) tracTickets'
    createPhabricatorTickets phabricatorTickets
    putStrLn $ "Migrated " ++ (show $ length tracTickets') ++ " tickets."


describeTicket :: TracTicket -> String
describeTicket ticket = T.unpack $
    T.concat [
        (t_summary ticket),
            "\n\tFields: ", (textLength $ t_customFields ticket),
            "\n\tComments:", (textLength $ t_comments ticket),
            "\n\n"
    ]
    where textLength = T.pack . show . length


tracTicketToPhabricatorTicket :: [PhabricatorUser] -> TracTicket -> ManiphestTicket
tracTicketToPhabricatorTicket users ticket =
    ManiphestTicket
        { m_title = (t_summary ticket)
        , m_description = convert <$> (t_description ticket)
        , m_ownerPHID = findUser <$> t_owner ticket
        , m_authorPHID = findUser $ t_reporter ticket
        , m_priority = convertPriority $ t_priority ticket
        , m_created = t_time $ ticket
        , m_modified = t_changetime $ ticket
        , m_phid = Nothing
        , m_status = t_status ticket
        , m_changes = map (tracCommentToPhabComment users) (t_comments ticket)
        }
    where findUser u = fromMaybe (traceShow ("Could not find", u) botUser) (lookupPhabricatorUserPHID users u)

tracCommentToPhabComment :: [PhabricatorUser] -> TracTicketComment -> ManiphestComment
tracCommentToPhabComment users (TracTicketComment{..})
  = ManiphestComment
      { mc_created = co_time
      , mc_comment = co_comment
      , mc_authorId = findUser co_author }
  where
    findUser u = fromMaybe (traceShow ("Could not find", u) botUser) (lookupPhabricatorUserPHID users u)


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
    fmap u_phid $ find (\x -> u_userName x == username) users
