module App.AppM where

import Prelude

import App.Capability.Navigate (class Navigate)
import App.Capability.Resource.User (class ManageUser)
import App.Data.Route (routeCodec)
import App.Env (Env)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Foreign (unsafeToForeign)
import Routing.Duplex as RD
import Type.Equality (class TypeEquals, from)


newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
    ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
    navigate route = do
        history <- asks _.history
        liftEffect $ history.pushState (unsafeToForeign {}) (RD.print routeCodec route)

instance manageUserAppM :: ManageUser AppM where
    loginUser username = do
        userEnv <- asks _.userEnv

        liftEffect $ Ref.write (Just username) userEnv.currentUser
        -- need to save user to localstorage too
        liftAff $ Bus.write (Just username) userEnv.userBus

        pure $ Just username