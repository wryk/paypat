module App.Page.Profile where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Component.HOC.Connect as Connect
import App.Component.HTML.Utils (safeHref)
import App.Data.Model (Username)
import App.Data.Route as Route
import App.Env (UserEnv)
import App.Form.Field as Field
import App.Form.Validation as V
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
    { currentUser :: Maybe Username
    , username :: Username
    }

data Action
    = Receive { username :: Username, currentUser :: Maybe Username }
    | HandleForm FormFields

type Query a
    = Const Void

type Input =
    { username :: Username
    }

type Output
    = Void

type ChildSlots =
    ( formless :: F.Slot TransactionForm (Const Void) () FormFields Unit
    )

component
    :: ∀ m r
    . MonadAff m
    => MonadAsk { userEnv :: UserEnv | r } m
    => Navigate m
    => H.Component HH.HTML (Const Void) Input Output m
component = Connect.component $ H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

    where
        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            Receive { currentUser } ->
                H.modify_ _ { currentUser = currentUser }

            HandleForm fields -> do
                { currentUser, username } <- H.get

                for_ currentUser \senderUsername ->
                    navigate $ Route.Transaction
                        { recipient: username
                        , sender: senderUsername
                        , amount: fields.amount
                        , date: "2019-08-26T17:09:4+02:00"
                        }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text $ "Profile : " <> (unwrap state.username) ]
                , case state.currentUser of
                    Nothing -> HH.a
                        [ safeHref $ Route.ProfileLogin state.username
                        ]
                        [ HH.text "Login to send headpats" ]
                    Just _ -> HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
                ]

type FormFields =
    { amount :: Int
    }

-- Form
newtype TransactionForm r f = TransactionForm (r
    ( amount :: f V.FormError String Int
    ))

derive instance newtypeTransactionForm :: Newtype (TransactionForm r f) _

formComponent :: ∀ m. MonadAff m => F.Component TransactionForm (Const Void) () Unit FormFields m
formComponent = F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleEvent = F.raiseResult
    }
    where
        formInput :: Unit -> F.Input' TransactionForm m
        formInput _ =
            { validators: TransactionForm
                { amount: V.required >>> V.int
                }
            , initialInputs: Nothing
            }

        proxies = F.mkSProxies (F.FormProxy :: _ TransactionForm)

        renderForm { form } =
            HH.form_
                [ HH.fieldset_
                    [ Field.input proxies.amount form
                        [ HP.placeholder "Quantity"
                        , HP.type_ HP.InputNumber
                        ]
                    , Field.submit "Send headpats"
                    ]
                ]