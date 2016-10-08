{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Phabricator where

import GHC.Generics
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.IO.Streams.List (toList)
import Database.MySQL.Base

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
    } deriving (Show)

newtype PHID p = PHID Text
    deriving (Show, Eq)

data PhabricatorUser = PhabricatorUser
    { u_phid         :: PHID PhabricatorUser
    , u_userName     :: Text
    } deriving (Show)


mysqlToUser :: [MySQLValue] -> Maybe PhabricatorUser
mysqlToUser values =
    case values of
        (x:y:[]) -> PhabricatorUser <$> (decodePHID x) <*> (decodeUserName y)
        _ -> Nothing

    where
        decodePHID p = case p of
            MySQLBytes v -> Just $ PHID $ decodeUtf8 v
            _ -> Nothing
        decodeUserName u = case u of
            MySQLText t -> Just t
            _ -> Nothing


mysqlToUsers :: [[MySQLValue]] -> [PhabricatorUser]
mysqlToUsers = catMaybes . (map mysqlToUser)


getPhabricatorUsers :: IO ([PhabricatorUser])
getPhabricatorUsers = do
    conn <- connect defaultConnectInfo {ciDatabase = "phabricator_user", ciPassword = "foobar", ciPort = 32773}
    (_, rawUsersStream) <- query_ conn "SELECT phid, userName FROM user"
    close conn
    rawUsers <- toList rawUsersStream
    let users = mysqlToUsers rawUsers
    putStrLn $ show users
    return []
