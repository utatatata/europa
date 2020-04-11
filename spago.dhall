{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "europa"
, dependencies =
    [ "ansi"
    , "arrays"
    , "console"
    , "effect"
    , "free"
    , "js-timers"
    , "psci-support"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
