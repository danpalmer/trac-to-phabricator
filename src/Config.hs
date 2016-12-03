{-# LANGUAGE OverloadedStrings #-}
module Config where

import Network.Conduit.Client
import qualified Database.MySQL.Base as M
import qualified Database.PostgreSQL.Simple as P
import Types

data ConduitConfig =
  ConduitConfig
    { authToken :: ConduitAuth }

-- The ID of the user doing the migration
botUser :: UserID
botUser = PHID "PHID-USER-cv4luanhibq47r6o2zrb"


conduitConfig :: ConduitConfig
--conduitConfig = ConduitConfig (ConduitAPITokenAuth "api-rltdytuu6tek4yspds6wshfslw3r")
conduitConfig = ConduitConfig (ConduitAPITokenAuth "api-obtum5xnbtckzkhg2v3xw63rdneb")

phabConnectInfo :: M.ConnectInfo
phabConnectInfo =
  M.defaultConnectInfo { M.ciDatabase = "bitnami_phabricator_user"
                       , M.ciPassword = "bitnami1"
                       , M.ciHost = "127.0.0.1"
                       , M.ciPort = 12345}

tracConnectInfo :: P.ConnectInfo
tracConnectInfo =
  P.defaultConnectInfo { P.connectDatabase = "trac_ghc"}

conduitAPIUrl = "http://192.168.1.11/api"

conduit :: Conduit
conduit = Conduit conduitAPIUrl (authToken conduitConfig)
