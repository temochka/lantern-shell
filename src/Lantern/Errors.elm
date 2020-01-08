module Lantern.Errors exposing (Error(..))


type Error
    = Error String


toString : Error -> String
toString (Error e) =
    e
