module App.Env where

import Routing.PushState (PushStateInterface)

type Env =
    { history :: PushStateInterface
    }
