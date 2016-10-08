{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib
    ( migrate
    ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Network.Conduit.Client

import Trac
import Phabricator


migrate :: IO ()
migrate = do
    let authToken = ConduitAPITokenAuth "api-gptwkv5kg4nayou7gl5zasm3u5hu"
    let conduit = Conduit "http://phabricator.dev/api" authToken
    users <- getUsers conduit
    tickets <- getTracTickets
    mapM (putStr . describeTicket) tickets
    putStrLn $ "Found " ++ (show $ length tickets) ++ " tickets."


describeTicket :: TracTicket -> String
describeTicket ticket = T.unpack $
    T.concat [
        (t_summary ticket),
            "\n\tFields: ", (textLength $ t_customFields ticket),
            "\n\tComments:", (textLength $ t_comments ticket),
            "\n\n"
    ]
    where textLength = T.pack . show . length
