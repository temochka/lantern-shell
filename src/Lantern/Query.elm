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


type Value
    = Null
    | Integer Int
    | Real Float
    | Text String


type alias SelectResult =
    Json.Decode.Value


valueDecoder : Json.Decode.Decoder Value
valueDecoder =
    Json.Decode.oneOf
        [ Json.Decode.float |> Json.Decode.map Real
        , Json.Decode.int |> Json.Decode.map Integer
        , Json.Decode.string |> Json.Decode.map Text
        , Json.Decode.null Null
        ]


withNoArguments : String -> Query
withNoArguments query =
    { source = query, arguments = Dict.empty }


decodeResult : SelectResult -> Json.Decode.Decoder query -> Result Json.Decode.Error query
decodeResult result decoder =
    Json.Decode.decodeValue decoder result
