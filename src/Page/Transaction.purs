module App.Page.Transaction where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Data.Model (Transaction, Username)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State =
    { transaction :: Transaction
    , currentUser :: Username
    }

data Action
    = Initialize

type Query a
    = Const Void

type Input
    = State

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
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

    where
        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                pure unit

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { currentUser, transaction } =
            HH.div_
                [ HH.h1_ [ HH.text "Transaction" ]
                , HH.p_ [ HH.text message ]
                ]

            where
                message
                    | currentUser == transaction.recipient = "You received " <> (show transaction.amount) <> " headpats from " <> (unwrap transaction.sender)
                    | currentUser == transaction.sender = "You sent "<> (show transaction.amount) <> " headpats to " <> (unwrap transaction.recipient)
                    | otherwise = "Nothing to see here"