module Lantern.Request exposing (..)

import Json.Encode
import Lantern.Query as Query


type alias Id =
    String


type Request
    = Echo String
    | Query Query.Query
    | Migration Query.Query


encode : String -> Request -> Json.Encode.Value
encode id request =
    case request of
        Echo text ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Echo" )
                , ( "text", Json.Encode.string text )
                ]

        Query query ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Query" )
                , ( "query", Json.Encode.string query.source )
                , ( "arguments", Json.Encode.dict identity encodeQueryArgument query.arguments )
                ]

        Migration ddl ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Migration" )
                , ( "ddl", Json.Encode.string ddl.source )
                ]


encodeQueryArgument : Query.Argument -> Json.Encode.Value
encodeQueryArgument arg =
    case arg of
        Query.Int i ->
            Json.Encode.string (String.fromInt i)

        Query.Float f ->
            Json.Encode.string (String.fromFloat f)

        Query.String s ->
            Json.Encode.string s
