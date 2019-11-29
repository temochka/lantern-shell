module Lantern exposing
    ( Connection
    , Error
    , LiveQuery
    , Message
    , RequestPort
    , Response
    , ResponsePort
    , echo
    , errorToString
    , liveQueries
    , log
    , migrate
    , newConnection
    , prepareLiveQuery
    , prepareLiveQuery2
    , prepareLiveQuery3
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
    , currentLiveQueryId : String
    , requestsInFlight : RequestsInFlight msg
    , requestPort : RequestPort msg
    , responsePort : ResponsePort msg
    , toMsg : Message msg -> msg
    , log : Log
    }


type Message msg
    = ResponseMsg Response
    | Request Lantern.Request.Request (ResponseHandler msg)


type alias ResponseHandler msg =
    Lantern.Response.Response -> List msg


type alias RequestsInFlight msg =
    Dict String (ResponseHandler msg)


type LiveQuery msg
    = LiveQuery (List Lantern.Query.Query) (List Lantern.Query.ReaderResult -> msg)


newConnection : RequestPort msg -> ResponsePort msg -> (Message msg -> msg) -> Connection msg
newConnection requestPort responsePort toMsg =
    Connection
        { requestId = 0
        , currentLiveQueryId = ""
        , requestsInFlight = Dict.empty
        , requestPort = requestPort
        , responsePort = responsePort
        , toMsg = toMsg
        , log = Log.new
        }


subscriptions : Connection msg -> Sub msg
subscriptions (Connection state) =
    state.responsePort (ResponseMsg >> state.toMsg)


echo : String -> (String -> msg) -> Connection msg -> Cmd msg
echo payload msg (Connection ({ toMsg } as state)) =
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
    Task.perform toMsg (Task.succeed (Request request handler))


readerQuery :
    Lantern.Query.Query
    -> Json.Decode.Decoder a
    -> (Result Error (List a) -> msg)
    -> Connection msg
    -> Cmd msg
readerQuery query decoder msg (Connection ({ toMsg } as state)) =
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
                        |> List.singleton

                _ ->
                    [ msg (Err (Error "Unexpected response")) ]
    in
    Task.perform toMsg (Task.succeed (Request request handler))


writerQuery :
    Lantern.Query.Query
    -> (Bool -> msg)
    -> Connection msg
    -> Cmd msg
writerQuery query msg (Connection ({ toMsg } as state)) =
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
    Task.perform toMsg (Task.succeed (Request request handler))


liveQueries : List (LiveQuery msg) -> Connection msg -> Cmd msg
liveQueries orderedQueries (Connection { toMsg }) =
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
    Task.perform toMsg (Task.succeed (Request request handler))


prepareLiveQuery :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> (Result Error (List a) -> msg)
    -> LiveQuery msg
prepareLiveQuery ( queryA, decoderA ) msg =
    let
        handler results =
            case results of
                [ result ] ->
                    result
                        |> Json.Decode.decodeValue (Json.Decode.list decoderA)
                        |> Result.mapError (Json.Decode.errorToString >> Error)
                        |> msg

                unexpectedResults ->
                    msg (Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length unexpectedResults))))
    in
    LiveQuery [ queryA ] handler


prepareLiveQuery2 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> (Result Error ( List a, List b ) -> msg)
    -> LiveQuery msg
prepareLiveQuery2 ( queryA, decoderA ) ( queryB, decoderB ) msg =
    let
        handler results =
            case results of
                [ resultA, resultB ] ->
                    Result.map2 Tuple.pair
                        (resultA |> Json.Decode.decodeValue (Json.Decode.list decoderA))
                        (resultB |> Json.Decode.decodeValue (Json.Decode.list decoderB))
                        |> Result.mapError (Json.Decode.errorToString >> Error)
                        |> msg

                unexpectedResults ->
                    msg (Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length unexpectedResults))))
    in
    LiveQuery [ queryA, queryB ] handler


prepareLiveQuery3 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> (Result Error ( List a, List b, List c ) -> msg)
    -> LiveQuery msg
prepareLiveQuery3 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) msg =
    let
        handler results =
            case results of
                [ resultA, resultB, resultC ] ->
                    Result.map3 (\a b c -> ( a, b, c ))
                        (resultA |> Json.Decode.decodeValue (Json.Decode.list decoderA))
                        (resultB |> Json.Decode.decodeValue (Json.Decode.list decoderB))
                        (resultC |> Json.Decode.decodeValue (Json.Decode.list decoderC))
                        |> Result.mapError (Json.Decode.errorToString >> Error)
                        |> msg

                unexpectedResults ->
                    msg (Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length unexpectedResults))))
    in
    LiveQuery [ queryA, queryB, queryC ] handler


migrate : Lantern.Query.Query -> (Bool -> msg) -> Connection msg -> Cmd msg
migrate query_ msg (Connection ({ toMsg } as state)) =
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
    Task.perform toMsg (Task.succeed (Request request handler))


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

        ResponseMsg payload ->
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


errorToString : Error -> String
errorToString (Error e) =
    e


log : Connection msg -> Log
log (Connection state) =
    state.log
