module Lantern exposing
    ( Connection
    , Error
    , Message
    , RequestPort
    , ResponsePort
    , echo
    , liveQueries
    , log
    , map
    , migrate
    , newConnection
    , readerQuery
    , subscriptions
    , update
    , writerQuery
    )

import Dict exposing (Dict)
import Element exposing (Element)
import Json.Decode
import Json.Encode
import Lantern.Decoders as Decoders
import Lantern.Encoders as Encoders
import Lantern.Errors
import Lantern.Extra.Result
import Lantern.LiveQuery exposing (LiveQuery(..))
import Lantern.Log as Log exposing (Log)
import Lantern.Query
import Lantern.Request
import Lantern.Response
import Task


type alias Error =
    Lantern.Errors.Error


type alias RequestPort msg =
    RawRequest -> Cmd msg


type alias ResponsePort msg =
    (RawResponse -> msg) -> Sub msg


type alias RawRequest =
    String


type alias RawResponse =
    String


type Connection msg
    = Connection (State msg)


type alias State msg =
    { requestId : Int
    , currentLiveQueryId : String
    , requestsInFlight : RequestsInFlight msg
    , requestPort : RequestPort msg
    , log : Log
    }


type Message msg
    = RawResponse RawResponse
    | Request Lantern.Request.Request (ResponseHandler msg)


type alias ResponseHandler msg =
    Lantern.Response.Response -> List msg


type alias RequestsInFlight msg =
    Dict String (ResponseHandler msg)


subscriptions : (Message msg -> msg) -> ResponsePort msg -> Sub msg
subscriptions wrapMsg responsePort =
    responsePort (RawResponse >> wrapMsg)


newConnection : RequestPort msg -> Connection msg
newConnection requestPort =
    Connection
        { requestId = 0
        , currentLiveQueryId = ""
        , requestsInFlight = Dict.empty
        , requestPort = requestPort
        , log = Log.new
        }


echo : String -> (String -> msg) -> Cmd (Message msg)
echo payload msg =
    let
        handler response =
            case response of
                Lantern.Response.Echo text ->
                    [ msg text ]

                _ ->
                    [ msg "Server error" ]

        request =
            Lantern.Request.Echo payload
    in
    Task.perform identity (Task.succeed (Request request handler))


readerQuery :
    Lantern.Query.Query
    -> Json.Decode.Decoder a
    -> (Result Error (List a) -> msg)
    -> Cmd (Message msg)
readerQuery query decoder msg =
    let
        request =
            Lantern.Request.ReaderQuery query

        handler response =
            case response of
                Lantern.Response.ReaderQuery result ->
                    result
                        |> Json.Decode.decodeValue (Json.Decode.list decoder)
                        |> Result.mapError (Json.Decode.errorToString >> Lantern.Errors.Error)
                        |> msg
                        |> List.singleton

                _ ->
                    [ msg (Err (Lantern.Errors.Error "Unexpected response")) ]
    in
    Task.perform identity (Task.succeed (Request request handler))


writerQuery :
    Lantern.Query.Query
    -> (Bool -> msg)
    -> Cmd (Message msg)
writerQuery query msg =
    let
        request =
            Lantern.Request.WriterQuery query

        handler response =
            case response of
                Lantern.Response.WriterQuery r ->
                    [ msg True ]

                _ ->
                    [ msg False ]
    in
    Task.perform identity (Task.succeed (Request request handler))


liveQueries : List (LiveQuery msg) -> Cmd (Message msg)
liveQueries orderedQueries =
    let
        request =
            orderedQueries
                |> List.map (\(LiveQuery queries _) -> queries)
                |> Lantern.Request.LiveQuery

        resultHandlers =
            orderedQueries
                |> List.indexedMap (\i (LiveQuery _ resultHandler) -> ( i, resultHandler ))
                |> Dict.fromList

        handler response =
            case response of
                Lantern.Response.LiveQuery results ->
                    results
                        |> List.indexedMap
                            (\i queryResults ->
                                Dict.get i resultHandlers
                                    |> Maybe.map (\resultHandler -> resultHandler queryResults)
                            )
                        |> List.filterMap identity

                _ ->
                    []
    in
    Task.perform identity (Task.succeed (Request request handler))


migrate : Lantern.Query.Query -> (Bool -> msg) -> Cmd (Message msg)
migrate query_ msg =
    let
        request =
            Lantern.Request.Migration query_

        handler response =
            case response of
                Lantern.Response.Migration ->
                    [ msg True ]

                _ ->
                    [ msg False ]
    in
    Task.perform identity (Task.succeed (Request request handler))


update : Message msg -> Connection msg -> ( Connection msg, Cmd msg )
update msg (Connection state) =
    case msg of
        Request request handler ->
            let
                ( requestsCounter, requestId ) =
                    ( state.requestId + 1, String.fromInt (state.requestId + 1) )

                isLiveQuery =
                    case request of
                        Lantern.Request.LiveQuery _ ->
                            True

                        _ ->
                            False

                invalidateOldHandler requestsInFlight =
                    if isLiveQuery then
                        Dict.remove state.currentLiveQueryId requestsInFlight

                    else
                        requestsInFlight

                newRequestsInFlight =
                    state.requestsInFlight
                        |> invalidateOldHandler
                        |> Dict.insert requestId handler

                newCurrentLiveQueryId =
                    if isLiveQuery then
                        requestId

                    else
                        state.currentLiveQueryId

                newState =
                    { state
                        | requestId = requestsCounter
                        , currentLiveQueryId = newCurrentLiveQueryId
                        , requestsInFlight = newRequestsInFlight
                        , log = Log.logRequest state.log requestId request
                    }
            in
            ( Connection newState
            , state.requestPort (Json.Encode.encode 0 (Encoders.request requestId request))
            )

        RawResponse payload ->
            let
                parsedResponse =
                    Json.Decode.decodeString Decoders.response payload
            in
            case parsedResponse of
                Ok ( id, response ) ->
                    let
                        handler =
                            Dict.get id state.requestsInFlight

                        newRequestsInFlight =
                            case response of
                                Lantern.Response.LiveQuery _ ->
                                    state.requestsInFlight

                                _ ->
                                    Dict.remove id state.requestsInFlight

                        newState =
                            { state
                                | requestsInFlight = newRequestsInFlight
                                , log = Log.logResponse state.log id response
                            }
                    in
                    case handler of
                        Just callback ->
                            ( Connection newState, callback response |> List.map (\r -> Task.perform identity (Task.succeed r)) |> Cmd.batch )

                        Nothing ->
                            ( Connection newState, Cmd.none )

                Err err ->
                    ( Connection { state | log = Log.log state.log Log.Error (Json.Decode.errorToString err) }, Cmd.none )


log : Connection msg -> Log
log (Connection state) =
    state.log


map : (a -> b) -> Message a -> Message b
map f msg =
    case msg of
        Request request handler ->
            Request request (\response -> List.map f (handler response))

        RawResponse response ->
            RawResponse response
