{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib
    ( migrate
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either
import GHC.Generics
import Database.PostgreSQL.Simple
import Network.Conduit.Client

import Trac
import Phabricator


migrate :: IO ()
migrate = do
    let authToken = ConduitAPITokenAuth "api-gptwkv5kg4nayou7gl5zasm3u5hu"
    let conduit = Conduit "http://phabricator.dev/api" authToken
    users <- getUsers conduit
    getTracTickets
    putStrLn $ show users


getUsers :: Conduit -> IO (Either Text [PhabricatorUser])
getUsers conduit = do
    response <- callConduitPairs conduit "user.query" []
    return $ case response of
        ConduitResult users -> Right users
        ConduitError code info -> Left (code `T.append` info)


getTracTickets :: IO ([TracTicket])
getTracTickets = do
    conn <- connect defaultConnectInfo {connectDatabase = "ghc_trac"}
    rawTickets <- query_ conn "SELECT * FROM ticket"
    customFields <- query_ conn "SELECT * FROM ticket_custom"
    ticketComments <- query_ conn "SELECT * FROM ticket_change WHERE field = 'comment'"
    return $ mergeTracData rawTickets customFields ticketComments
