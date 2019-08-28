module App.Form.Validation where

import Prelude

import App.Data.Transaction (Username(..))
import Data.Either (Either(..))
import Formless as F

data FormError
    = Required
    | InvalidUsername

errorToString :: FormError -> String
errorToString = case _ of
    Required -> "This field is required."
    InvalidUsername -> "Invalid username"

required :: ∀ form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

usernameFormat :: ∀ form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ map Username <<< cond (_ /= mempty) InvalidUsername

cond :: ∀ a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err