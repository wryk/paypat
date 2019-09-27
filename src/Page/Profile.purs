module App.Page.Profile where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Model (Username)
import App.Data.Route as Route
import App.Form.Field as Field
import App.Form.Validation as V
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
    { username :: Username
    }

data Action
    = HandleForm FormFields

type Query a
    = Const Void

type Input
    = Username

type Output
    = Void

type ChildSlots =
    ( formless :: F.Slot TransactionForm (Const Void) () FormFields Unit
    )

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
        }
    }

    where
        initialState :: Input -> State
        initialState username =
            { username
            }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            HandleForm fields -> do
                recipient <- H.gets _.username

                navigate $ Route.SenderReceipt
                    { recipient
                    , sender: fields.username
                    , amount: fields.amount
                    , date: "2019-08-26T17:09:4+02:00"
                    }

        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text $ "Profile :" <> (unwrap state.username) ]
                , HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
                ]

type FormFields =
    { username :: Username
    , amount :: Int
    }

-- Form
newtype TransactionForm r f = TransactionForm (r
    ( username :: f V.FormError String Username
    , amount :: f V.FormError String Int
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
                { username: V.required >>> V.usernameFormat
                , amount: V.required >>> V.int
                }
            , initialInputs: Nothing
            }


        proxies = F.mkSProxies (F.FormProxy :: _ TransactionForm)

        renderForm { form } =
            HH.form_
                [ HH.fieldset_
                    [ Field.input proxies.username form
                        [ HP.placeholder "Username"
                        , HP.type_ HP.InputText
                        ]
                    , Field.input proxies.amount form
                        [ HP.placeholder "Quantity"
                        , HP.type_ HP.InputNumber
                        ]
                    , Field.submit "Send headpats"
                    ]
                ]