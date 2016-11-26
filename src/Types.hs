{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Types where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Trac.Convert as T

import Network.Conduit.Client

convert :: Text -> Text
convert = T.pack . T.convert . T.unpack

-- Used as a kind
data PHIDType = Ticket | Author | Transaction

type ManiphestTicketPHID = PHID 'Ticket
type ManiphestTransactionPHID = PHID 'Transaction
type ManiphestAuthorPHID = PHID 'Author

unwrapPHID :: PHID a -> Text
unwrapPHID (PHID t) = t
