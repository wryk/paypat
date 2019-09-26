module App.Data.Model where

import Prelude

import Control.Bind (bindFlipped)
import Data.Either (Either, hush, note)
import Data.Newtype (class Newtype)
import Data.String.Base64 (decode, encodeUrl)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

newtype Username = Username String

derive instance newtypeUsername :: Newtype Username _
derive newtype instance showUsername :: Show Username
derive newtype instance eqUsername :: Eq Username
derive newtype instance ordUsername :: Ord Username
derive newtype instance readForeignUsername :: ReadForeign Username
derive newtype instance writeForeignUsername :: WriteForeign Username

type Transaction =
    { sender :: Username
    , recipient :: Username
    , amount :: Int
    , date :: String
    }

encodeUrlTransaction :: Transaction -> String
encodeUrlTransaction = writeJSON >>> encodeUrl

decodeUrlTransaction :: String -> Either String Transaction
decodeUrlTransaction = decode >>> hush >>> (bindFlipped (readJSON >>> hush)) >>> (note "Not a url encoded Transaction")
