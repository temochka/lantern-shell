module Lantern exposing (Message, RequestPort, Response, ResponsePort, State, echo, init, query, subscriptions, update)

import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Lantern.Query
import Lantern.Request
import Lantern.Response
import Task


type alias RequestPort msg =
    String -> Cmd msg


type alias ResponsePort msg =
    (String -> msg) -> Sub msg


type alias Response =
    String


type alias State msg =
    { requestId : Int
    , requestsInFlight : RequestsInFlight msg
    , requestPort : RequestPort msg
    , responsePort : ResponsePort msg
    , updater : Message -> msg
    , echoResponses : List ( String, String )
    , selectQueryResponses : List ( String, Lantern.Query.SelectResult )
    }


type Message
    = ResponseMsg Response


type alias RequestsInFlight msg =
    { echo : Dict String (String -> msg)
    , query : Dict String (List Json.Decode.Value -> msg)
    }


emptyRequests : RequestsInFlight msg
emptyRequests =
    { echo = Dict.empty
    , query = Dict.empty
    }


init : RequestPort msg -> ResponsePort msg -> (Message -> msg) -> ( State msg, Cmd msg )
init requestPort responsePort updater =
    ( { requestId = 0
      , requestsInFlight = emptyRequests
      , requestPort = requestPort
      , responsePort = responsePort
      , updater = updater
      , echoResponses = []
      , selectQueryResponses = []
      }
    , Cmd.none
    )


subscriptions : State msg -> Sub msg
subscriptions state =
    state.responsePort (ResponseMsg >> state.updater)


echo : State msg -> String -> (String -> msg) -> ( State msg, Cmd msg )
echo ({ requestsInFlight } as state) payload handler =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        newRequestsInFlight =
            { requestsInFlight | echo = Dict.insert requestId handler requestsInFlight.echo }

        newState =
            { state | requestId = requestsCounter, requestsInFlight = newRequestsInFlight }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Lantern.Request.encode (String.fromInt newState.requestId) (Lantern.Request.Echo payload))) )


query : State msg -> Lantern.Query.Query -> (List Json.Decode.Value -> msg) -> ( State msg, Cmd msg )
query ({ requestsInFlight } as state) query_ handler =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        newRequestsInFlight =
            { requestsInFlight | query = Dict.insert requestId handler requestsInFlight.query }

        newState =
            { state | requestId = requestsCounter, requestsInFlight = newRequestsInFlight }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Lantern.Request.encode (String.fromInt newState.requestId) (Lantern.Request.Query query_))) )


update : Message -> State msg -> ( State msg, Cmd msg )
update msg ({ requestsInFlight } as state) =
    case msg of
        ResponseMsg payload ->
            let
                response =
                    Json.Decode.decodeString Lantern.Response.decoder payload
            in
            case response of
                Ok ( id, Lantern.Response.Echo text ) ->
                    let
                        handler =
                            Dict.get id requestsInFlight.echo

                        newRequestsInFlight =
                            { requestsInFlight | echo = Dict.remove id requestsInFlight.echo }

                        newState =
                            { state | echoResponses = ( id, text ) :: state.echoResponses, requestsInFlight = newRequestsInFlight }
                    in
                    case handler of
                        Just callback ->
                            ( newState, Task.perform callback (Task.succeed text) )

                        Nothing ->
                            ( newState, Cmd.none )

                Ok ( id, Lantern.Response.Query r ) ->
                    let
                        handler =
                            Dict.get id requestsInFlight.query

                        newRequestsInFlight =
                            { requestsInFlight | query = Dict.remove id requestsInFlight.query }

                        newState =
                            { state | selectQueryResponses = ( id, r ) :: state.selectQueryResponses, requestsInFlight = newRequestsInFlight }
                    in
                    case handler of
                        Just callback ->
                            ( newState, Task.perform callback (Task.succeed r) )

                        Nothing ->
                            ( newState, Cmd.none )

                Ok ( id, Lantern.Response.Unknown _ ) ->
                    Debug.todo "do something"

                Err err ->
                    let
                        _ =
                            Debug.log "parse error:" err
                    in
                    ( state, Cmd.none )
