{-# LANGUAGE DeriveGeneric #-}

module Trac where

import GHC.Generics
import Data.Text (Text)
import Control.Monad (liftM)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock (picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.PostgreSQL.Simple.FromRow

data TracTicket = TracTicket
    { t_id :: Int
    , t_type :: Text
    , t_time :: UTCTime
    , t_changetime :: UTCTime
    , t_component :: Text
    , t_severity :: Maybe Text
    , t_priority :: Text
    , t_owner :: Maybe Text
    , t_reporter :: Text
    , t_cc :: Maybe Text
    , t_version :: Maybe Text
    , t_milestone :: Maybe Text
    , t_status :: Text
    , t_resolution :: Maybe Text
    , t_summary :: Text
    , t_description :: Maybe Text
    , t_keywords :: Maybe Text
    } deriving (Generic, Show)

instance FromRow TracTicket where
    fromRow = TracTicket <$> field
                         <*> field
                         <*> (liftM tracTimeToUTCTime field)
                         <*> (liftM tracTimeToUTCTime field)
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field
                         <*> field


tracTimeToUTCTime :: Integer -> UTCTime
tracTimeToUTCTime = posixSecondsToUTCTime . toNominalDiffTime . picosecondsToDiffTime
    where toNominalDiffTime = fromRational . toRational
