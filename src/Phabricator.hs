{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator where

import GHC.Generics
import Data.Monoid ((<>))
import Data.Aeson.TH
import Data.Text (Text)
import Network.Conduit.Client

data ManiphestAPITicket = ManiphestAPITicket
    { m_title :: String
    , m_description :: Maybe String
    , m_ownerPHID :: Maybe String
    , m_viewPolicy :: Maybe String
    , m_editPolicy :: Maybe String
    , m_ccPHIDs :: Maybe [String]
    , m_priority :: Maybe Int
    , m_projectPHIDs :: Maybe [String]
    , m_auxiliary :: Maybe String
    } deriving (Generic, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''ManiphestAPITicket)


data PhabricatorUser = PhabricatorUser
    { p_image        :: !Url
    , p_phid         :: !(PHID User)
    , p_realName     :: !Text
    , p_roles        :: [Text]
    , p_uri          :: !Url
    , p_userName     :: !Text
    } deriving (Show,Generic)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''PhabricatorUser)


getUsers :: Conduit -> IO (Either Text [PhabricatorUser])
getUsers conduit = do
    response <- callConduitPairs conduit "user.query" []
    return $ case response of
        ConduitResult users -> Right users
        ConduitError code info -> Left (code <> info)
