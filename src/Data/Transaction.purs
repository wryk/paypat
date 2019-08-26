module App.Data.Transaction where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String.Base64 (decode, encodeUrl)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

newtype Username = Username String

derive instance newtypeUsername :: Newtype Username _
derive newtype instance showUsername :: Show Username
derive newtype instance eqUsername :: Eq Username
derive newtype instance ordUsername :: Ord Username
derive newtype instance readForeignUsername :: ReadForeign Username
derive newtype instance writeForeignUsername :: WriteForeign Username

newtype Payload = Payload String

derive instance newtypePayload :: Newtype Payload _
derive newtype instance showPayload :: Show Payload
derive newtype instance eqPayload :: Eq Payload
derive newtype instance ordPayload :: Ord Payload
derive newtype instance readForeignPayload :: ReadForeign Payload
derive newtype instance writeForeignPayload :: WriteForeign Payload

type Transaction =
    { sender :: Username
    , recipient :: Username
    , amount :: Int
    , date :: String
    }

encodePayload :: Transaction -> Payload
encodePayload = writeJSON >>> encodeUrl >>> wrap

-- decodePayload :: Payload -> Maybe Transaction
-- decodePayload payload = do
--     let
--         base64 = unwrap payload

--         eitherJson = hush <<< decode $ base64
