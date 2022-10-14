module Lantern.Errors exposing (Error(..), toString)


type Error
    = Error String


toString : Error -> String
toString (Error e) =
    e
