module App.Utils where

import Prelude

import App.Data.Route (Route)
import App.Data.Route as Route
import Effect (Effect)
import Effect.Class (liftEffect)
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