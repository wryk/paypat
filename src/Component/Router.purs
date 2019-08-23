module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Route (Route(..), routeCodec)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Web.HTML (window)
import Web.HTML.Location as WL
import Web.HTML.Window as Window

type State =
    { route :: Maybe Route
    }

data Action
    = Initialize

data Query a
    = Navigate Route a

type Input
    = Unit

type Output
    = Void

type ChildSlots =
    ()

component
    :: ∀ m
     . MonadAff m
    => Navigate m
    => H.Component HH.HTML Query Input Output m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }

    where
        initialState :: Input -> State
        initialState _ = { route: Nothing }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
        handleAction = case _ of
            Initialize -> do
                initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getPath
                navigate $ fromMaybe Home initialRoute

        handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
        handleQuery = case _ of
            Navigate destination a -> do
                { route } <- H.get

                when (route /= Just destination) do
                    H.modify_ _ { route = Just destination }

                pure $ Just a

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { route } =
            HH.main_
                [ case route of
                    Just r -> case r of
                        Home ->
                            HH.text "Home"
                        Profile username ->
                            HH.text $ username <> "'s profile"
                    Nothing ->
                        HH.text "404"
                ]

getPath :: Effect String
getPath = window >>= Window.location >>= WL.pathname