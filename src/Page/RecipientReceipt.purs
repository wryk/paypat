module App.Page.RecipientReceipt where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Data.Model (Transaction)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
    { transaction :: Transaction
    }

data Action
    = Initialize

type Query a
    = Const Void

type Input
    = Transaction

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
        initialState transaction =
            { transaction
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                pure unit

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { transaction } =
            HH.div_
                [ HH.h1_ [ HH.text "Transaction" ]
                , HH.p_ [ HH.text $ (unwrap transaction.recipient) <>" received " <> (show transaction.amount) <> " headpats from " <> (unwrap transaction.sender) ]
                , HH.p_ [ HH.text $ show transaction ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    ]
                    [ HH.text "Give back headpats to SENDER !"
                    ]
                ]