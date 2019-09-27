module App.Utils where

import Prelude

import App.Data.Route (Route)
import App.Data.Route as Route
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen.HTML as HH
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Location (host, protocol)
import Web.HTML.Window (location)

generateUrl :: Route -> String
generateUrl = print Route.routeCodec

generateAbsoluteUrl :: Route -> Effect String
generateAbsoluteUrl route = do
    location <- liftEffect $ window >>= location
    protocol <- liftEffect $ protocol location
    host <- liftEffect $ host location

    pure $ protocol <> host <> (generateUrl route)

maybeElement :: ∀ p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElement (Just x) f = f x
maybeElement _ _ = HH.text ""

whenElement :: ∀ p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElement cond f = if cond then f unit else HH.text ""