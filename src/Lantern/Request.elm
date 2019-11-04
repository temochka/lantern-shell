module Lantern.Request exposing (..)

import Json.Encode
import Lantern.Query as Query


type Request
    = Echo String
    | Query Query.Query


encode : Int -> Request -> Json.Encode.Value
encode id request =
    case request of
        Echo text ->
            Json.Encode.object
                [ ( "id", Json.Encode.string <| String.fromInt id )
                , ( "type", Json.Encode.string "Echo" )
                , ( "text", Json.Encode.string text )
                ]

        Query query ->
            Json.Encode.object
                [ ( "id", Json.Encode.string <| String.fromInt id )
                , ( "type", Json.Encode.string "Query" )
                , ( "query", Json.Encode.string query.source )
                ]
