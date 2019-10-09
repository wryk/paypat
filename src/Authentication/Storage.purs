module App.Authentication.Storage where

import Prelude

import App.Data.Model (Username(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)


usernameKey :: String
usernameKey = "username"

readUsername :: Effect (Maybe Username)
readUsername = do
    string <- getItem usernameKey =<< localStorage =<< window
    pure $ map Username string

writeUsername :: Username -> Effect Unit
writeUsername (Username string) =
    setItem usernameKey string =<< localStorage =<< window

removeUsername :: Effect Unit
removeUsername =
    removeItem usernameKey =<< localStorage =<< window
