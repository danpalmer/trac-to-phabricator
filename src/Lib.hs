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
    users <- getPhabricatorUsers
    tickets <- getTracTickets
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
