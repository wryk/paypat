module App.Env where

import App.Data.Model (Username)
import Data.Maybe (Maybe)
import Effect.Aff.Bus (BusRW)
import Effect.Ref (Ref)
import Routing.PushState (PushStateInterface)

type Env =
    { history :: PushStateInterface
    , userEnv :: UserEnv
    }

type UserEnv =
    { currentUser :: Ref (Maybe Username)
    , userBus :: BusRW (Maybe Username)
    }
