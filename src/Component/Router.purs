module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Capability.Resource.User (class ManageUser)
import App.Component.HOC.Connect (WithCurrentUser)
import App.Component.HOC.Connect as Connect
import App.Component.HTML.Header (header)
import App.Component.Utils (OpaqueSlot)
import App.Data.Model (Username)
import App.Data.Route (Route(..), routeCodec)
import App.Env (UserEnv)
import App.Page.Home as PageHome
import App.Page.Login as PageLogin
import App.Page.Profile as PageProfile
import App.Page.Transaction as PageTransaction
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Web.HTML (window)
import Web.HTML.Location as WL
import Web.HTML.Window as Window

type State =
    { route :: Maybe Route
    , currentUser :: Maybe Username
    }

data Action
    = Initialize
    | Receive { | WithCurrentUser () }

data Query a
    = Navigate Route a

type Output
    = Void

type ChildSlots =
    ( home :: OpaqueSlot Unit
    , login :: OpaqueSlot Unit
    , profile :: OpaqueSlot Unit
    , transaction :: OpaqueSlot Unit
    )

component
    :: ∀ m r
     . MonadAff m
    => MonadAsk { userEnv :: UserEnv | r } m
    => Navigate m
    => ManageUser m
    => H.Component HH.HTML Query {} Void m
component = Connect.component $ H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

    where
        initialState { currentUser } = { route: Nothing, currentUser }

        handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
        handleAction = case _ of
            Initialize -> do
                initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getPath
                navigate $ fromMaybe Home initialRoute
                    where
                        getPath :: Effect String
                        getPath = window >>= Window.location >>= WL.pathname

            Receive { currentUser } ->
                H.modify_ _ { currentUser = currentUser }

        handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
        handleQuery = case _ of
            Navigate destination a -> do
                { route, currentUser } <- H.get

                when (route /= Just destination) do
                    case (isJust currentUser && destination `elem` [ Login ]) of
                        false -> H.modify_ _ { route = Just destination }
                        _ -> pure unit

                pure $ Just a

        authorize :: Maybe Username -> H.ComponentHTML Action ChildSlots m -> H.ComponentHTML Action ChildSlots m
        authorize mbProfile html = case mbProfile of
            Nothing ->
                HH.slot (SProxy :: _ "login") unit PageLogin.component { redirect: Nothing } absurd
            Just _ ->
                html

        render :: State -> H.ComponentHTML Action ChildSlots m
        render { route, currentUser } = case route of
            Just r ->
                HH.main_
                [ header currentUser r
                , case r of
                    Home ->
                        HH.slot (SProxy :: _ "home") unit PageHome.component {} absurd
                    Login ->
                        HH.slot (SProxy :: _ "login") unit PageLogin.component { redirect: Just Home } absurd
                    (Profile username) ->
                        HH.slot (SProxy :: _ "profile") unit PageProfile.component { username } absurd
                    (ProfileLogin username) ->
                        HH.slot (SProxy :: _ "login") unit PageLogin.component { redirect: Just $ Profile username } absurd
                    (Transaction transaction) ->
                        case currentUser of
                            Nothing ->
                                HH.slot (SProxy :: _ "login") unit PageLogin.component { redirect: Nothing } absurd
                            Just user ->
                                HH.slot (SProxy :: _ "transaction") unit PageTransaction.component { transaction, currentUser: user } absurd
                ]

            Nothing ->
                HH.text "404"