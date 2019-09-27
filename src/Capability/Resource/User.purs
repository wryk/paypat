module App.Capability.Resource.User where

import Prelude

import App.Data.Model (Username)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)

class Monad m <= ManageUser m where
    loginUser :: Username -> m (Maybe Username)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
    loginUser = lift <<< loginUser
