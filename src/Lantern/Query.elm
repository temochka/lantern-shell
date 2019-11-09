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
    Json.Decode.Value


withNoArguments : String -> Query
withNoArguments query =
    { source = query, arguments = Dict.empty }


decodeResult : SelectResult -> Json.Decode.Decoder query -> Result Json.Decode.Error query
decodeResult result decoder =
    Json.Decode.decodeValue decoder result
