module App.Page.Home where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Route as Route
import App.Data.Transaction (Username(..))
import App.Form.Field as Field
import App.Form.Validation as V
import App.Utils (whenElement)
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
    Unit

data Action
    = HandleForm FormFields

type Query a
    = Const Void

type Input
    = Unit

type Output
    = Void

type ChildSlots =
  ( formless :: F.Slot ProfileForm (Const Void) () FormFields Unit
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
        initialState _ = unit

        handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
        handleAction = case _ of
            HandleForm fields -> do
                H.liftEffect $ logShow fields
                navigate $ Route.Profile fields.username

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
newtype ProfileForm r f = ProfileForm (r
    ( username :: f V.FormError String Username
    ))

derive instance newtypeProfileForm :: Newtype (ProfileForm r f) _

formComponent :: ∀ m. MonadAff m => F.Component ProfileForm (Const Void) () Unit FormFields m
formComponent = F.component formInput $ F.defaultSpec
    { render = renderForm
    , handleEvent = F.raiseResult
    }
    where
        formInput :: Unit -> F.Input' ProfileForm m
        formInput _ =
            { validators: ProfileForm
                { username: V.required >>> V.usernameFormat
                }
            , initialInputs: Nothing
            }


        proxies = F.mkSProxies (F.FormProxy :: _ ProfileForm)

        renderForm { form } =
            HH.form_
                [ HH.fieldset_
                    [ Field.input proxies.username form
                        [ HP.placeholder "Username"
                        , HP.type_ HP.InputText
                        ]
                    , Field.submit "Go to profile"
                    ]
                ]