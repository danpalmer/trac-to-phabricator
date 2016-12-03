{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Trac.Convert as T

import Network.Conduit.Client hiding (User, Repo)

convert :: Text -> Text
convert = T.pack . T.convert . T.unpack

-- Used as a kind
data PHIDType = Ticket | User | Transaction | Diff | Project | Repo | Commit


type TicketID = PHID 'Ticket
type TransactionID = PHID 'Transaction
type UserID        = PHID 'User
type DiffID        = PHID 'Diff
type ProjectID     = PHID 'Project
type CommitID     = PHID 'Commit

unwrapPHID :: PHID a -> Text
unwrapPHID (PHID t) = t

data KeywordType = Arch | Keyword | OS | Milestone | Component | Type  deriving Show
