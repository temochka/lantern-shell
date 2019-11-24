module Lantern.Query exposing (..)

import Dict exposing (Dict)
import Json.Decode


type Argument
    = Int Int
    | Float Float
    | String String


type alias Arguments =
    Dict String Argument


type alias Query =
    { source : String
    , arguments : Arguments
    }


type Value
    = Null
    | Integer Int
    | Real Float
    | Text String


type alias ReaderResult =
    Json.Decode.Value


type alias WriterResult =
    { changedRows : Int
    , lastInsertRowId : Int
    }


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


decodeResult : ReaderResult -> Json.Decode.Decoder query -> Result Json.Decode.Error query
decodeResult result decoder =
    Json.Decode.decodeValue decoder result
