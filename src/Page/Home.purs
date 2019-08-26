module App.Page.Home where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Route as Route
import App.Data.Transaction (Username(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
    { username :: Maybe Username
    }

data Action
    = Initialize
    | Demo

type Query a
    = Const Void

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
    => H.Component HH.HTML (Const Void) Input Output m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

    where
        initialState :: Input -> State
        initialState _ =
            { username: Nothing
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                H.modify_ _ { username = Just $ Username "tixie" }

            Demo -> do
                maybeUsername <- H.gets _.username

                case maybeUsername of
                    Nothing ->
                        pure unit
                    Just username ->
                        navigate $ Route.Profile username

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "Home" ]
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.value $ maybe "" show state.username
                    , HP.readOnly true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.disabled $ isNothing state.username
                    , HE.onClick \_ -> Just Demo
                    ]
                    [ HH.text "Go to profile"
                    ]
                ]