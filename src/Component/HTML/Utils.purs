module App.Component.HTML.Utils where

import Prelude

import App.Data.Route (Route, routeCodec)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

safeHref :: ∀ r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< print routeCodec

maybeElement :: ∀ p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElement (Just x) f = f x
maybeElement _ _ = HH.text ""

whenElement :: ∀ p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElement cond f = if cond then f unit else HH.text ""