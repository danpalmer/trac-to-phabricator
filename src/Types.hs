{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Trac.Convert as T

import Network.Conduit.Client hiding (User, Repo)

convert :: Int -> T.CommentMap -> Text -> Text
convert n cm = T.pack . T.convert n cm . T.unpack

type CommentMap = T.CommentMap

-- Used as a kind
data PHIDType = Ticket | User | Transaction | Diff | Project | Repo | Commit | File


type TicketID = PHID 'Ticket
type TransactionID = PHID 'Transaction
type UserID        = PHID 'User
type DiffID        = PHID 'Diff
type ProjectID     = PHID 'Project
type CommitID      = PHID 'Commit
type FileID        = PHID 'File

unwrapPHID :: PHID a -> Text
unwrapPHID (PHID t) = t

data KeywordType = Arch | Keyword | OS | Milestone | Component | Type  deriving Show
