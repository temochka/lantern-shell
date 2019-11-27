module Lantern exposing
    ( Connection
    , Error
    , Message
    , RequestPort
    , Response
    , ResponsePort
    , andThen
    , echo
    , errorToString
    , init
    , liveQuery2
    , log
    , migrate
    , readerQuery
    , subscriptions
    , update
    , writerQuery
    )

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Lantern.Decoders as Decoders
import Lantern.Encoders as Encoders
import Lantern.Extra.Result
import Lantern.Log as Log exposing (Log)
import Lantern.Query
import Lantern.Request
import Lantern.Response
import Task


type Error
    = Error String


type alias RequestPort msg =
    String -> Cmd msg


type alias ResponsePort msg =
    (String -> msg) -> Sub msg


type alias Response =
    String


type Connection msg
    = Connection (State msg)


type alias State msg =
    { requestId : Int
    , requestsInFlight : RequestsInFlight msg
    , requestPort : RequestPort msg
    , responsePort : ResponsePort msg
    , updater : Message -> msg
    , log : Log
    }


type Message
    = ResponseMsg Response


type alias RequestsInFlight msg =
    Dict String (Lantern.Response.Response -> msg)


init : RequestPort msg -> ResponsePort msg -> (Message -> msg) -> ( Connection msg, Cmd msg )
init requestPort responsePort updater =
    ( Connection
        { requestId = 0
        , requestsInFlight = Dict.empty
        , requestPort = requestPort
        , responsePort = responsePort
        , updater = updater
        , log = Log.new
        }
    , Cmd.none
    )


andThen : (Connection msg -> ( Connection msg, Cmd msg )) -> ( Connection msg, Cmd msg ) -> ( Connection msg, Cmd msg )
andThen thenFn ( connection, cmd ) =
    let
        ( newState, newCmd ) =
            thenFn connection
    in
    ( newState, Cmd.batch [ cmd, newCmd ] )


subscriptions : Connection msg -> Sub msg
subscriptions (Connection state) =
    state.responsePort (ResponseMsg >> state.updater)


echo : Connection msg -> String -> (String -> msg) -> ( Connection msg, Cmd msg )
echo (Connection ({ requestsInFlight } as state)) payload msg =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        handler response =
            case response of
                Lantern.Response.Echo text ->
                    msg text

                _ ->
                    msg "Server error"

        newRequestsInFlight =
            Dict.insert requestId handler requestsInFlight

        request =
            Lantern.Request.Echo payload

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( Connection newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


readerQuery :
    Connection msg
    -> Lantern.Query.Query
    -> Json.Decode.Decoder a
    -> (Result Error (List a) -> msg)
    -> ( Connection msg, Cmd msg )
readerQuery (Connection ({ requestsInFlight } as state)) query decoder msg =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.ReaderQuery query

        handler response =
            case response of
                Lantern.Response.ReaderQuery result ->
                    result
                        |> Json.Decode.decodeValue (Json.Decode.list decoder)
                        |> Result.mapError (Json.Decode.errorToString >> Error)
                        |> msg

                _ ->
                    msg (Err (Error "Unexpected response"))

        newRequestsInFlight =
            Dict.insert requestId handler requestsInFlight

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( Connection newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


writerQuery :
    Connection msg
    -> Lantern.Query.Query
    -> (Bool -> msg)
    -> ( Connection msg, Cmd msg )
writerQuery (Connection ({ requestsInFlight } as state)) query msg =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.WriterQuery query

        handler response =
            case response of
                Lantern.Response.WriterQuery r ->
                    msg True

                _ ->
                    msg False

        newRequestsInFlight =
            Dict.insert requestId handler requestsInFlight

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( Connection newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


liveQuery_ : List Lantern.Query.Query -> (Lantern.Response.Response -> msg) -> Connection msg -> ( Connection msg, Cmd msg )
liveQuery_ queries liveResultHandler (Connection state) =
    let
        requestId =
            "LiveQuery"

        request =
            Lantern.Request.LiveQuery queries

        newRequestsInFlight =
            Dict.insert requestId liveResultHandler state.requestsInFlight

        newState =
            { state
                | log = Log.logRequest state.log requestId request
                , requestsInFlight = newRequestsInFlight
            }
    in
    ( Connection newState, state.requestPort (Json.Encode.encode 0 (Encoders.request requestId request)) )


liveQuery :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> (Result Error (List a) -> msg)
    -> Connection msg
    -> ( Connection msg, Cmd msg )
liveQuery ( queryA, decoderA ) msg connection =
    let
        handler response =
            case response of
                Lantern.Response.LiveQuery [ result ] ->
                    result
                        |> Json.Decode.decodeValue (Json.Decode.list decoderA)
                        |> Result.mapError (Json.Decode.errorToString >> Error)
                        |> msg

                Lantern.Response.LiveQuery results ->
                    msg (Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results))))

                _ ->
                    msg (Err (Error "unexpected response"))
    in
    liveQuery_ [ queryA ] handler connection


liveQuery2 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> (List a -> List b -> result)
    -> (Result Error result -> msg)
    -> Connection msg
    -> ( Connection msg, Cmd msg )
liveQuery2 ( queryA, decoderA ) ( queryB, decoderB ) resultConstructor msg connection =
    let
        handler response =
            case response of
                Lantern.Response.LiveQuery [ resultA, resultB ] ->
                    Result.map2 resultConstructor
                        (resultA |> Json.Decode.decodeValue (Json.Decode.list decoderA) |> Result.mapError (Json.Decode.errorToString >> Error))
                        (resultB |> Json.Decode.decodeValue (Json.Decode.list decoderB) |> Result.mapError (Json.Decode.errorToString >> Error))
                        |> msg

                Lantern.Response.LiveQuery results ->
                    msg (Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results))))

                _ ->
                    msg (Err (Error "unexpected response"))
    in
    liveQuery_ [ queryA, queryB ] handler connection


migrate : Lantern.Query.Query -> (Bool -> msg) -> Connection msg -> ( Connection msg, Cmd msg )
migrate query_ msg (Connection ({ requestsInFlight } as state)) =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.Migration query_

        handler response =
            case response of
                Lantern.Response.Migration ->
                    msg True

                _ ->
                    msg False

        newRequestsInFlight =
            Dict.insert requestId handler requestsInFlight

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( Connection newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


update : Message -> Connection msg -> ( Connection msg, Cmd msg )
update msg (Connection ({ requestsInFlight } as state)) =
    case msg of
        ResponseMsg payload ->
            let
                parsedResponse =
                    Json.Decode.decodeString Decoders.response payload
            in
            case parsedResponse of
                Ok ( id, response ) ->
                    let
                        handler =
                            Dict.get id requestsInFlight

                        newRequestsInFlight =
                            Dict.remove id requestsInFlight

                        newState =
                            { state
                                | requestsInFlight = newRequestsInFlight
                                , log = Log.logResponse state.log id response
                            }
                    in
                    case handler of
                        Just callback ->
                            ( Connection newState, Task.perform callback (Task.succeed response) )

                        Nothing ->
                            ( Connection newState, Cmd.none )

                Err err ->
                    ( Connection { state | log = Log.log state.log Log.Error (Json.Decode.errorToString err) }, Cmd.none )


errorToString : Error -> String
errorToString (Error e) =
    e


log : Connection msg -> Log
log (Connection state) =
    state.log
