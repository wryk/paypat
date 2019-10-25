module App.Component.Header where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.User (class ManageUser, logoutUser)
import App.Component.HOC.Connect as Connect
import App.Component.HTML.Utils (safeHref)
import App.Data.Model (Username)
import App.Data.Route (Route)
import App.Data.Route as Route
import App.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State =
    { currentUser :: Maybe Username
    , route :: Route
    }

data Action
    = Receive { currentUser :: Maybe Username, route :: Route }
    | NavigatePrevent MouseEvent Route
    | Logout

type Query a
    = Const Void

type Input =
    { route :: Route
    }

type Output
    = Void

component
    :: ∀ m r
    . MonadAff m
    => MonadAsk { userEnv :: UserEnv | r } m
    => Navigate m
    => ManageUser m
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
        handleAction :: Action -> H.HalogenM State Action () Void m Unit
        handleAction = case _ of
            Receive { currentUser } ->
                H.modify_ _ { currentUser = currentUser }

            NavigatePrevent mouseEvent route -> do
                H.liftEffect $ preventDefault $ toEvent mouseEvent
                navigate route

            Logout -> do
                logoutUser

        render :: State -> H.ComponentHTML Action () m
        render { currentUser, route } =
            HH.header_
                [ HH.section_
                    [ linkItem Route.Home [ HH.text "Paypat" ]
                    ]
                , HH.section_
                    case currentUser of
                        Nothing ->
                            [ HH.text "Not connected"
                            , linkItem Route.Login [ HH.text "Login" ]
                            ]

                        Just username ->
                            [ HH.text $ "Connected as " <> (unwrap username)
                            , HH.button
                                [ HP.type_ HP.ButtonButton
                                , HE.onClick \_ -> Just Logout
                                ]
                                [ HH.text "Logout"
                                ]
                            ]
                ]

            where
                linkItem r html =
                    HH.a
                        [ safeHref r
                        , HE.onClick \event -> Just $ NavigatePrevent event r
                        ]
                        html