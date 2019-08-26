module App.Page.Profile where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Route as Route
import App.Data.Transaction (Username(..), encodePayload)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
    { username :: Username
    , sender :: Maybe Username
    , amount :: Int
    }

data Action
    = Initialize
    | Demo

type Query a
    = Const Void

type Input
    = Username

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
        initialState username =
            { username
            , sender: Nothing
            , amount: 0
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                H.modify_ _ { sender = Just $ Username "milia", amount = 5 }

            Demo -> do
                { username, amount } <- H.get

                maybeSender <- H.gets _.sender

                case maybeSender of
                    Nothing ->
                        pure unit
                    Just sender -> do
                        let
                            transaction =
                                { sender
                                , recipient: username
                                , amount
                                , date: "2019-08-26T17:09:4+02:00"
                                }

                            payload = encodePayload transaction

                        navigate $ Route.SenderReceipt payload

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text $ "Profile : " <> (show state.username) ]
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.value $ maybe "" show state.sender
                    , HP.readOnly true
                    ]
                , HH.input
                    [ HP.type_ HP.InputNumber
                    , HP.value $ show state.amount
                    , HP.readOnly true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HE.onClick \_ -> Just Demo
                    ]
                    [ HH.text "Send headpats"
                    ]
                ]