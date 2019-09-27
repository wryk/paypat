module App.Page.Login where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.User (class ManageUser, loginUser)
import App.Data.Route as Route
import App.Data.Model (Username)
import App.Form.Field as Field
import App.Form.Validation as V
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
    { redirect :: Boolean
    }

data Action
    = HandleForm FormFields

type Query a
    = Const Void

type Input =
    { redirect :: Boolean
    }

type Output
    = Void

type ChildSlots =
  ( formless :: F.Slot LoginForm (Const Void) () FormFields Unit
  )

component
    :: ∀ m
    . MonadAff m
    => Navigate m
    => ManageUser m
    => H.Component HH.HTML (Const Void) Input Output m
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

    where
        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            HandleForm fields -> do
                loginResult <- loginUser fields.username

                case loginResult of
                    Nothing -> do
                        pure unit
                    Just username -> do
                        redirect <- H.gets _.redirect
                        when redirect (navigate Route.Home)


        render :: State -> H.ComponentHTML Action ChildSlots m
        render state =
            HH.div_
                [ HH.h1_ [ HH.text "Home" ]
                , HH.slot F._formless unit formComponent unit (Just <<< HandleForm)
                ]

type FormFields =
    { username :: Username
    }

-- Form
newtype LoginForm r f = LoginForm (r
    ( username :: f V.FormError String Username
    ))

derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

formComponent :: ∀ m. MonadAff m => F.Component LoginForm (Const Void) () Unit FormFields m
formComponent = F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleEvent = F.raiseResult
    }
    where
        formInput :: Unit -> F.Input' LoginForm m
        formInput _ =
            { validators: LoginForm
                { username: V.required >>> V.usernameFormat
                }
            , initialInputs: Nothing
            }

        proxies = F.mkSProxies (F.FormProxy :: _ LoginForm)

        renderForm { form } =
            HH.form_
                [ HH.fieldset_
                    [ Field.input proxies.username form
                        [ HP.placeholder "Username"
                        , HP.type_ HP.InputText
                        ]
                    , Field.submit "Login"
                    ]
                ]