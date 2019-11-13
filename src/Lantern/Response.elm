module Lantern.Response exposing (..)

import Json.Decode
import Lantern.Query
import Lantern.Request


type Response
    = Echo String
    | Query Lantern.Query.SelectResult
    | Migration
    | Unknown Json.Decode.Value


decoder : Json.Decode.Decoder ( String, Response )
decoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string
            |> Json.Decode.andThen
                (\requestType ->
                    case requestType of
                        "Query" ->
                            Json.Decode.field "results" Json.Decode.value
                                |> Json.Decode.map Query

                        "Echo" ->
                            Json.Decode.field "text" Json.Decode.string
                                |> Json.Decode.map Echo

                        "Migration" ->
                            Json.Decode.succeed Migration

                        _ ->
                            Json.Decode.map Unknown Json.Decode.value
                )
        )
