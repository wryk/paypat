module App.Utils where

import Prelude

import App.Data.Route (Route)
import App.Data.Route as Route
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Routing.Duplex (print)

generateUrl :: Route -> String
generateUrl = print Route.routeCodec

maybeElement :: ∀ p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElement (Just x) f = f x
maybeElement _ _ = HH.text ""

whenElement :: ∀ p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElement cond f = if cond then f unit else HH.text ""