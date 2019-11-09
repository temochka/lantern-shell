module Lantern exposing (Error, Message, RequestPort, Response, ResponsePort, State, echo, errorToString, init, query, subscriptions, update)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
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


type alias State query msg =
    { requestId : Int
    , requestsInFlight : RequestsInFlight query msg
    , requestPort : RequestPort msg
    , responsePort : ResponsePort msg
    , updater : Message -> msg
    , log : Log
    }


type Message
    = ResponseMsg Response


type alias RequestsInFlight query msg =
    { echo : Dict String (String -> msg)
    , query : Dict String ( Json.Decode.Decoder query, Result Error query -> msg )
    }


emptyRequests : RequestsInFlight query msg
emptyRequests =
    { echo = Dict.empty
    , query = Dict.empty
    }


init : RequestPort msg -> ResponsePort msg -> (Message -> msg) -> ( State query msg, Cmd msg )
init requestPort responsePort updater =
    ( { requestId = 0
      , requestsInFlight = emptyRequests
      , requestPort = requestPort
      , responsePort = responsePort
      , updater = updater
      , log = Log.new
      }
    , Cmd.none
    )


subscriptions : State query msg -> Sub msg
subscriptions state =
    state.responsePort (ResponseMsg >> state.updater)


echo : State query msg -> String -> (String -> msg) -> ( State query msg, Cmd msg )
echo ({ requestsInFlight } as state) payload handler =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        newRequestsInFlight =
            { requestsInFlight | echo = Dict.insert requestId handler requestsInFlight.echo }

        request =
            Lantern.Request.Echo payload

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Lantern.Request.encode (String.fromInt newState.requestId) request)) )


query : State query msg -> Lantern.Query.Query -> Json.Decode.Decoder query -> (Result Error query -> msg) -> ( State query msg, Cmd msg )
query ({ requestsInFlight } as state) query_ decoder msg =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.Query query_

        handler =
            ( decoder, msg )

        newRequestsInFlight =
            { requestsInFlight | query = Dict.insert requestId handler requestsInFlight.query }

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Lantern.Request.encode (String.fromInt newState.requestId) request)) )


update : Message -> State query msg -> ( State query msg, Cmd msg )
update msg ({ requestsInFlight } as state) =
    case msg of
        ResponseMsg payload ->
            let
                response =
                    Json.Decode.decodeString Lantern.Response.decoder payload
            in
            case response of
                Ok ( id, (Lantern.Response.Echo text) as serverResponse ) ->
                    let
                        handler =
                            Dict.get id requestsInFlight.echo

                        newRequestsInFlight =
                            { requestsInFlight | echo = Dict.remove id requestsInFlight.echo }

                        newState =
                            { state
                                | requestsInFlight = newRequestsInFlight
                                , log = Log.logResponse state.log id serverResponse
                            }
                    in
                    case handler of
                        Just callback ->
                            ( newState, Task.perform callback (Task.succeed text) )

                        Nothing ->
                            ( newState, Cmd.none )

                Ok ( id, (Lantern.Response.Query r) as serverResponse ) ->
                    let
                        handler =
                            Dict.get id requestsInFlight.query

                        newRequestsInFlight =
                            { requestsInFlight | query = Dict.remove id requestsInFlight.query }

                        newState =
                            { state
                                | requestsInFlight = newRequestsInFlight
                                , log = Log.logResponse state.log id serverResponse
                            }
                    in
                    case handler of
                        Just ( decoder, callback ) ->
                            let
                                parseResult =
                                    Lantern.Query.decodeResult r decoder
                                        |> Result.mapError (Json.Decode.errorToString >> Error)
                            in
                            ( newState, Task.perform callback (Task.succeed parseResult) )

                        Nothing ->
                            ( newState, Cmd.none )

                Ok ( id, serverResponse ) ->
                    ( { state | log = Log.logResponse state.log id serverResponse }, Cmd.none )

                Err err ->
                    ( { state | log = Log.log state.log Log.Error (Json.Decode.errorToString err) }, Cmd.none )


errorToString : Error -> String
errorToString (Error e) =
    e
