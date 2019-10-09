module Main where

import Prelude

import App.AppM (runAppM)
import App.Authentication.Storage (readUsername)
import App.Component.Router as Router
import App.Data.Route (Route, routeCodec)
import App.Env (Env, UserEnv)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Bus as Bus
import Effect.Ref as Ref
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
    currentUser <- liftEffect $ Ref.new Nothing
    userBus <- liftEffect Bus.make

    liftEffect do
        mbUsername <- readUsername
        Ref.write mbUsername currentUser

    let
        userEnv :: UserEnv
        userEnv =
            { currentUser
            , userBus
            }

        env :: Env
        env =
            { history
            , userEnv
            }

        rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
        rootComponent = H.hoist (runAppM env) Router.component

    body <- HA.awaitBody
    halogenIO <- runUI rootComponent {} body

    let
        navigation :: Maybe Route -> Route -> Effect Unit
        navigation old new = do
            when (old /= Just new) do
                launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

    void $ liftEffect $ RP.matchesWith (RD.parse routeCodec) navigation history