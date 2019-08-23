module Main where

import Prelude

import App.AppM (runAppM)
import App.Component.Router as Router
import App.Data.Route (Route, routeCodec)
import App.Env (Env)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.PushState as RP

main :: Effect Unit
main = HA.runHalogenAff do
    history <- liftEffect RP.makeInterface
    body <- HA.awaitBody

    let
        env :: Env
        env =
            { history
            }

        rootComponent :: H.Component HH.HTML Router.Query Router.Input Router.Output Aff
        rootComponent = H.hoist (runAppM env) Router.component

    halogenIO <- runUI rootComponent unit body

    let
        navigation :: Maybe Route -> Route -> Effect Unit
        navigation old new = do
            when (old /= Just new) do
                launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

    void $ liftEffect $ RP.matchesWith (RD.parse routeCodec) navigation history