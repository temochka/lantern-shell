module Lantern.Decoders exposing (response)

import Dict
import Json.Decode
import Lantern.Query as Query
import Lantern.Response as Response


writerQueryResult : Json.Decode.Decoder Query.WriterResult
writerQueryResult =
    Json.Decode.map2 Query.WriterResult
        (Json.Decode.field "changed_rows" Json.Decode.int)
        (Json.Decode.field "last_insert_rowid" Json.Decode.int)


response : Json.Decode.Decoder ( String, Response.Response )
response =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string
            |> Json.Decode.andThen
                (\requestType ->
                    case requestType of
                        "ReaderQuery" ->
                            Json.Decode.field "results" Json.Decode.value
                                |> Json.Decode.map Response.ReaderQuery

                        "WriterQuery" ->
                            Json.Decode.map Response.WriterQuery
                                (Json.Decode.field "results" writerQueryResult)

                        "LiveQuery" ->
                            Json.Decode.field "results" (Json.Decode.dict Json.Decode.value)
                                |> Json.Decode.map (Dict.toList >> List.sortBy Tuple.first >> List.map Tuple.second >> Response.LiveQuery)

                        "Echo" ->
                            Json.Decode.field "text" Json.Decode.string
                                |> Json.Decode.map Response.Echo

                        "Migration" ->
                            Json.Decode.succeed Response.Migration

                        _ ->
                            Json.Decode.map Response.Unknown Json.Decode.value
                )
        )
