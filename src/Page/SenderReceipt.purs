module App.Page.SenderReceipt where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Data.Model (Transaction)
import App.Data.Route as Route
import App.Utils (generateAbsoluteUrl)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State =
    { transaction :: Transaction
    , recipientReceiptUrl :: Maybe String
    }

data Action
    = Initialize
    | HandleInput Input

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
        , receive = Just <<< HandleInput
        }
    }

    where
        initialState :: Input -> State
        initialState transaction = do
            { transaction
            , recipientReceiptUrl: Nothing
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Initialize -> do
                transaction <- H.gets _.transaction
                url <- H.liftEffect $ generateAbsoluteUrl $ Route.RecipientReceipt transaction
                H.modify_ _ { recipientReceiptUrl = Just url }

            HandleInput transaction -> do
                oldTransaction <- H.gets _.transaction

                when (oldTransaction /= transaction) do
                    url <- H.liftEffect $ generateAbsoluteUrl $ Route.RecipientReceipt transaction
                    H.modify_ _ { transaction = transaction, recipientReceiptUrl = Just url }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { transaction, recipientReceiptUrl } =
            HH.div_
                [ HH.h1_ [ HH.text "Transaction" ]
                , HH.p_ [ HH.text $ "You have successfully sent " <> (show transaction.amount) <> " headpats to " <> (unwrap transaction.recipient) ]
                , case recipientReceiptUrl of
                    Nothing ->
                        HH.p_ [ HH.text "Generating link, please wait" ]
                    Just url ->
                        HH.div_
                            [ HH.p_ [ HH.text "Share the following link with them !" ]
                            , HH.pre_ [ HH.text url ]
                            ]
                ]