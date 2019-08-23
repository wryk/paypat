module App.Data.Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)

data Route
    = Home
    | Profile String

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
    { "Home": noArgs
    , "Profile": segment
    }