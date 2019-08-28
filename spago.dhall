{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "b64"
    , "console"
    , "effect"
    , "halogen"
    , "halogen-formless"
    , "newtype"
    , "profunctor-lenses"
    , "psci-support"
    , "remotedata"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
