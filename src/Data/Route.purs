module App.Data.Route where

import Prelude hiding ((/))

import App.Data.Model (Transaction, Username, decodeUrlTransaction, encodeUrlTransaction)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', root, segment, as)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
    = Home
    | Login
    | Profile Username
    | ProfileLogin Username
    | Transaction Transaction

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
    show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Login": "login" / noArgs
    , "Profile": username
    , "ProfileLogin": username / "login"
    , "Transaction": "transaction" / transaction segment
    }

username :: RouteDuplex' Username
username = _Newtype segment

transaction :: RouteDuplex' String -> RouteDuplex' Transaction
transaction = as encodeUrlTransaction decodeUrlTransaction