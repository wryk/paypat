module App.Component.HOC.Connect where

import Prelude

import App.Component.Utils (busEventSource)
import App.Data.Model (Username)
import App.Env (UserEnv)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref as Ref
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Prim.Row as Row
import Record as Record

data Action output
    = Initialize
    | HandleUserBus (Maybe Username)
    | Emit output

type WithCurrentUser r =
    ( currentUser :: Maybe Username | r
    )

type ChildSlots query output =
    ( inner :: H.Slot query output Unit
    )

_inner = SProxy :: SProxy "inner"

component
    :: ∀ query input output m r
    . MonadAff m
    => MonadAsk { userEnv :: UserEnv | r } m
    => Row.Lacks "currentUser" input
    => H.Component HH.HTML query { | WithCurrentUser input } output m
    -> H.Component HH.HTML query { | input } output m
component innerComponent =
    H.mkComponent

    { initialState: Record.insert (SProxy :: _ "currentUser") Nothing
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        }
    }
    where
        handleAction = case _ of
            Initialize -> do
                { currentUser, userBus } <- asks _.userEnv
                _ <- H.subscribe (HandleUserBus <$> busEventSource userBus)
                maybeCurrentUser <- liftEffect $ Ref.read currentUser
                H.modify_ _ { currentUser = maybeCurrentUser }

            HandleUserBus maybeCurrentUser ->
                H.modify_ _ { currentUser = maybeCurrentUser }

            Emit output ->
                H.raise output

        handleQuery :: ∀ a. query a -> H.HalogenM _ _ _ _ _ (Maybe a)
        handleQuery = H.query _inner unit

        render state =
            HH.slot _inner unit innerComponent state (Just <<< Emit)
