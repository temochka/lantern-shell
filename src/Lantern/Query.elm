module Lantern.Query exposing (..)

import Dict exposing (Dict)
import Json.Decode


type Argument
    = Int Int
    | Float Float
    | String String


type alias Query =
    { source : String
    , arguments : Dict String Argument
    }


type alias SelectResult =
    List Json.Decode.Value


withNoArguments : String -> Query
withNoArguments query =
    { source = query, arguments = Dict.empty }
