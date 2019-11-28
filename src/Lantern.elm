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
    , liveQuery2
    , log
    , migrate
    , newConnection
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
    , updater : Message msg -> msg
    , log : Log
    }


type Message msg
    = ResponseMsg Response
    | Request Lantern.Request.Request (Lantern.Response.Response -> msg)


type alias RequestsInFlight msg =
    Dict String (Lantern.Response.Response -> msg)


newConnection : RequestPort msg -> ResponsePort msg -> (Message msg -> msg) -> Connection msg
newConnection requestPort responsePort updater =
    Connection
        { requestId = 0
        , requestsInFlight = Dict.empty
        , requestPort = requestPort
        , responsePort = responsePort
        , updater = updater
        , log = Log.new
        }


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


echo : String -> (String -> msg) -> Connection msg -> Cmd msg
echo payload msg (Connection ({ updater } as state)) =
    let
        handler response =
            case response of
                Lantern.Response.Echo text ->
                    msg text

                _ ->
                    msg "Server error"

        request =
            Lantern.Request.Echo payload
    in
    Task.perform updater (Task.succeed (Request request handler))


readerQuery :
    Lantern.Query.Query
    -> Json.Decode.Decoder a
    -> (Result Error (List a) -> msg)
    -> Connection msg
    -> Cmd msg
readerQuery query decoder msg (Connection ({ updater } as state)) =
    let
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
    in
    Task.perform updater (Task.succeed (Request request handler))


writerQuery :
    Lantern.Query.Query
    -> (Bool -> msg)
    -> Connection msg
    -> Cmd msg
writerQuery query msg (Connection ({ updater } as state)) =
    let
        request =
            Lantern.Request.WriterQuery query

        handler response =
            case response of
                Lantern.Response.WriterQuery r ->
                    msg True

                _ ->
                    msg False
    in
    Task.perform updater (Task.succeed (Request request handler))


liveQuery_ : List Lantern.Query.Query -> (Lantern.Response.Response -> msg) -> Connection msg -> Cmd msg
liveQuery_ queries handler (Connection { updater }) =
    let
        request =
            Lantern.Request.LiveQuery queries
    in
    Task.perform updater (Task.succeed (Request request handler))


liveQuery :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> (Result Error (List a) -> msg)
    -> Connection msg
    -> Cmd msg
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
    -> Cmd msg
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


migrate : Lantern.Query.Query -> (Bool -> msg) -> Connection msg -> Cmd msg
migrate query_ msg (Connection ({ updater } as state)) =
    let
        request =
            Lantern.Request.Migration query_

        handler response =
            case response of
                Lantern.Response.Migration ->
                    msg True

                _ ->
                    msg False
    in
    Task.perform updater (Task.succeed (Request request handler))


update : Message msg -> Connection msg -> ( Connection msg, Cmd msg )
update msg (Connection ({ requestsInFlight } as state)) =
    case msg of
        Request request handler ->
            let
                ( requestsCounter, requestId ) =
                    case request of
                        Lantern.Request.LiveQuery _ ->
                            ( state.requestId, "LiveQuery" )

                        _ ->
                            ( state.requestId + 1, String.fromInt (state.requestId + 1) )

                newRequestsInFlight =
                    Dict.insert requestId handler requestsInFlight

                newState =
                    { state
                        | requestId = requestsCounter
                        , requestsInFlight = newRequestsInFlight
                        , log = Log.logRequest state.log requestId request
                    }
            in
            ( Connection newState
            , state.requestPort (Json.Encode.encode 0 (Encoders.request requestId request))
            )

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
