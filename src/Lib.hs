{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib
    ( migrate
    ) where

import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Trac
import Phabricator


migrate :: IO ()
migrate = do
    phabricatorUsers <- getPhabricatorUsers
    tracTickets <- getTracTickets
    let tracTickets' = take 3 tracTickets
    let phabricatorTickets = map (tracTicketToPhabricatorTicket phabricatorUsers) tracTickets'
    phabricatorTickets' <- createPhabricatorTickets phabricatorTickets
    updatePhabricatorTickets phabricatorTickets'
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
        , m_description = t_description ticket
        , m_ownerPHID = findUser =<< t_owner ticket
        , m_authorPHID = findUser $ t_reporter ticket
        , m_priority = convertPriority $ t_priority ticket
        , m_created = t_time $ ticket
        , m_modified = t_changetime $ ticket
        , m_phid = Nothing
        , m_status = t_status ticket
        }
    where findUser = lookupPhabricatorUserPHID users

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
    fmap u_phid $ listToMaybe $ filter (\x -> u_userName x == username) users
