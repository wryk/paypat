module App.Data.Route where

import Prelude hiding ((/))

import App.Data.Transaction (Username, Payload)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
    = Home
    | Profile Username
    | SenderReceipt Payload
    | RecipientReceipt Payload

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
    show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Profile": username
    , "SenderReceipt": "sreceipt" / payload
    , "RecipientReceipt": "rreceipt" / payload
    }

username :: RouteDuplex' Username
username = _Newtype segment

payload :: RouteDuplex' Payload
payload = _Newtype segment