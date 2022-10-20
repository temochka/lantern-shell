module Lantern.Encoders exposing (query, queryArguments, request)

import Dict
import Json.Encode
import Lantern.Query as Query
import Lantern.Request as Request


query : Query.Query -> Json.Encode.Value
query { source, arguments } =
    Json.Encode.object
        [ ( "query", Json.Encode.string source )
        , ( "arguments", queryArguments arguments )
        ]


queryArguments : Query.Arguments -> Json.Encode.Value
queryArguments arguments =
    Json.Encode.dict
        identity
        (\v ->
            case v of
                Query.Integer i ->
                    Json.Encode.string <| String.fromInt i

                Query.Real f ->
                    Json.Encode.string <| String.fromFloat f

                Query.Text s ->
                    Json.Encode.string s

                Query.Null ->
                    Json.Encode.null
        )
        arguments


request : String -> Request.Request -> Json.Encode.Value
request id encodedRequest =
    case encodedRequest of
        Request.Nop ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Nop" )
                ]

        Request.Echo text ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Echo" )
                , ( "text", Json.Encode.string text )
                ]

        Request.ReaderQuery q ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "ReaderQuery" )
                , ( "query", query q )
                ]

        Request.WriterQuery q ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "WriterQuery" )
                , ( "query", query q )
                ]

        Request.LiveQuery liveQueries ->
            let
                taggedQuery tag index q =
                    ( String.fromInt tag ++ "/" ++ String.fromInt index, q )

                queryDict =
                    liveQueries
                        |> List.indexedMap Tuple.pair
                        |> List.concatMap (\( tag, queries ) -> List.indexedMap (taggedQuery tag) queries)
                        |> Dict.fromList
            in
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "LiveQuery" )
                , ( "queries", Json.Encode.dict identity query queryDict )
                ]

        Request.HttpRequest { body, headers, url, method } ->
            let
                payload =
                    Json.Encode.object
                        [ ( "headers", Json.Encode.list (\( name, value ) -> Json.Encode.list Json.Encode.string [ name, value ]) headers )
                        , ( "body"
                          , case body of
                                Just string ->
                                    Json.Encode.string string

                                Nothing ->
                                    Json.Encode.null
                          )
                        , ( "url", Json.Encode.string url )
                        , ( "method", Json.Encode.string method )
                        ]
            in
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "HttpRequest" )
                , ( "request", payload )
                ]

        Request.Migration ddl ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Migration" )
                , ( "ddl", Json.Encode.string ddl.source )
                ]
