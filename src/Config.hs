{-# LANGUAGE OverloadedStrings #-}
module Config where

import Network.Conduit.Client
import Data.Text
import qualified Database.MySQL.Base as M
import qualified Database.PostgreSQL.Simple as P

data ConduitConfig =
  ConduitConfig
    { authToken :: ConduitAuth }


conduitConfig :: ConduitConfig
conduitConfig = ConduitConfig (ConduitAPITokenAuth "api-rltdytuu6tek4yspds6wshfslw3r")

phabConnectInfo :: M.ConnectInfo
phabConnectInfo =
  M.defaultConnectInfo { M.ciDatabase = "bitnami_phabricator_user"
                       , M.ciPassword = "bitnami1"
                       , M.ciHost = "127.0.0.1"
                       , M.ciPort = 12345}

tracConnectInfo =
  P.defaultConnectInfo { P.connectDatabase = "trac_ghc"}

conduitAPIUrl = "http://192.168.1.3/api"


conduit = Conduit conduitAPIUrl (authToken conduitConfig)
