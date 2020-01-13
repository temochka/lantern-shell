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
                        "Nop" ->
                            Json.Decode.succeed Response.Nop

                        "Hello" ->
                            Json.Decode.succeed Response.Hello

                        "ReaderQuery" ->
                            Json.Decode.field "results" Json.Decode.value
                                |> Json.Decode.map Response.ReaderQuery

                        "WriterQuery" ->
                            Json.Decode.map Response.WriterQuery
                                (Json.Decode.field "results" writerQueryResult)

                        "LiveQuery" ->
                            Json.Decode.field "results" (Json.Decode.dict Json.Decode.value)
                                |> Json.Decode.map (dictToLiveQueryResponse >> Response.LiveQuery)

                        "Echo" ->
                            Json.Decode.field "text" Json.Decode.string
                                |> Json.Decode.map Response.Echo

                        "Migration" ->
                            Json.Decode.succeed Response.Migration

                        "FatalError" ->
                            Json.Decode.map Response.FatalError
                                (Json.Decode.field "error" Json.Decode.string)

                        _ ->
                            Json.Decode.map Response.Unknown Json.Decode.value
                )
        )


dictToLiveQueryResponse : Dict.Dict String Json.Decode.Value -> List (List Query.ReaderResult)
dictToLiveQueryResponse dict =
    let
        parseTag key =
            key
                |> String.split "/"
                |> (\l ->
                        case l of
                            [ tag, number ] ->
                                tag |> String.toInt |> Maybe.andThen (\numericTag -> String.toInt number |> Maybe.map (Tuple.pair numericTag))

                            _ ->
                                Nothing
                   )

        groupSortedResponses : List ( ( Int, Int ), Query.ReaderResult ) -> List (List Query.ReaderResult)
        groupSortedResponses responses =
            case responses of
                ( ( tag, _ ), _ ) :: _ ->
                    let
                        ( head, tail ) =
                            List.partition (\( ( otherTag, _ ), _ ) -> tag == otherTag) responses
                    in
                    List.map Tuple.second head :: groupSortedResponses tail

                _ ->
                    []
    in
    dict
        |> Dict.toList
        |> List.map (\( key, result ) -> parseTag key |> Maybe.map (\tag -> Tuple.pair tag result))
        |> List.filterMap identity
        |> List.sortBy Tuple.first
        |> groupSortedResponses
