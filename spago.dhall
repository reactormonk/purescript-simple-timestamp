{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "my-project"
, dependencies =
  [ "aff"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "formatters"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "partial"
  , "prelude"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "strings"
  , "transformers"
  ]
, packages = ./packages.dhall
, license = "MIT"
, repository = "git@github.com:reactormonk/purescript-simple-timestamp.git"
}
