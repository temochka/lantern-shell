module Lantern.Query exposing (..)

import Dict exposing (Dict)


type Argument
    = Int Int
    | Float Float
    | String String


type alias Query =
    { source : String
    , arguments : Dict String Argument
    }


withNoArguments : String -> Query
withNoArguments query =
    { source = query, arguments = Dict.empty }
