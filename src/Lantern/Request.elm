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
                ]

        Migration ddl ->
            Json.Encode.object
                [ ( "id", Json.Encode.string id )
                , ( "type", Json.Encode.string "Migration" )
                , ( "ddl", Json.Encode.string ddl.source )
                ]
