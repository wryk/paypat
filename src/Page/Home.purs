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
    { username :: Maybe Username
    }

data Action
    = Initialize
    | Demo
    | HandleForm FormFields

type Query a
    = Const Void

type Input
    = Unit

type Output
    = Void

type ChildSlots =
  ( formless :: F.Slot ProfileForm FormQuery () FormFields Unit
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

            HandleForm fields -> do
                H.liftEffect $ logShow fields

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
                    , HP.value $ maybe "" unwrap state.username
                    , HP.readOnly true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.disabled $ isNothing state.username
                    , HE.onClick \_ -> Just Demo
                    ]
                    [ HH.text "Go to profile"
                    ]
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

data FormQuery a
  = SetLoginError Boolean a

derive instance functorFormQuery :: Functor (FormQuery)

formComponent :: ∀ m. MonadAff m => F.Component ProfileForm FormQuery () Unit FormFields m
formComponent = F.component formInput $ F.defaultSpec
    { render = renderLogin
    , handleEvent = handleEvent
    , handleQuery = handleQuery
    }
    where
        formInput :: Unit -> F.Input ProfileForm (formError :: Boolean) m
        formInput _ =
            { validators: ProfileForm
                { username: V.required >>> V.usernameFormat
                }
            , initialInputs: Nothing
            , formError: false
            }

        handleEvent = F.raiseResult

        handleQuery :: ∀ a. FormQuery a -> H.HalogenM _ _ _ _ _ (Maybe a)
        handleQuery = case _ of
            SetLoginError bool a -> do
                H.modify_ _ { formError = bool }
                pure (Just a)

        proxies = F.mkSProxies (F.FormProxy :: _ ProfileForm)

        renderLogin { form, formError } =
            HH.form_
                [ whenElement formError \_ ->
                    HH.div_
                        [ HH.text "Error" ]
                    , HH.fieldset_
                        [ Field.input proxies.username form
                            [ HP.placeholder "Username"
                            , HP.type_ HP.InputText
                            ]
                        , Field.submit "Go to profile"
                    ]
                ]