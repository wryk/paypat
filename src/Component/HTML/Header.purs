module App.Component.HTML.Header where

import Prelude

import App.Component.HTML.Utils (safeHref)
import App.Data.Route (Route)
import App.Data.Route as Route
import App.Data.Model (Username)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Halogen.HTML as HH

header :: ∀ i p. Maybe Username -> Route -> HH.HTML i p
header currentUser currentRoute =
    HH.header_
        [ HH.text $ maybe "Not connected" (\username -> "Connected as " <> (unwrap username)) currentUser
        , case currentUser of
            Nothing ->
                linkItem Route.Login [ HH.text "Login" ]
            Just username ->
                linkItem (Route.Profile username) [ HH.text "Your profile" ]
        ]

    where
        linkItem route html =
            HH.a
                [ safeHref route
                ]
                html

