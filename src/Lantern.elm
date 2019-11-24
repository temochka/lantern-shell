module Lantern exposing
    ( Error
    , Message
    , RequestPort
    , Response
    , ResponsePort
    , State
    , andThen
    , echo
    , errorToString
    , init
    , liveQuery
    , liveQuery10
    , liveQuery2
    , liveQuery3
    , liveQuery4
    , liveQuery5
    , liveQuery6
    , liveQuery7
    , liveQuery8
    , liveQuery9
    , migrate
    , query
    , subscriptions
    , update
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


type alias LiveResultHandler liveQuery msg =
    { decodeResults : List Json.Decode.Value -> Result Error liveQuery
    , msg : Result Error liveQuery -> msg
    }


type alias State query liveQuery msg =
    { requestId : Int
    , requestsInFlight : RequestsInFlight query msg
    , liveResultHandler : Maybe (LiveResultHandler liveQuery msg)
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
    , migration : Dict String (Bool -> msg)
    }


emptyRequests : RequestsInFlight query msg
emptyRequests =
    { echo = Dict.empty
    , query = Dict.empty
    , migration = Dict.empty
    }


init : RequestPort msg -> ResponsePort msg -> (Message -> msg) -> ( State query liveQuery msg, Cmd msg )
init requestPort responsePort updater =
    ( { requestId = 0
      , requestsInFlight = emptyRequests
      , liveResultHandler = Nothing
      , requestPort = requestPort
      , responsePort = responsePort
      , updater = updater
      , log = Log.new
      }
    , Cmd.none
    )


andThen : (State query liveQuery msg -> ( State query liveQuery msg, Cmd msg )) -> ( State query liveQuery msg, Cmd msg ) -> ( State query liveQuery msg, Cmd msg )
andThen thenFn ( state, cmd ) =
    let
        ( newState, newCmd ) =
            thenFn state
    in
    ( newState, Cmd.batch [ cmd, newCmd ] )


subscriptions : State query liveQuery msg -> Sub msg
subscriptions state =
    state.responsePort (ResponseMsg >> state.updater)


echo : State query liveQuery msg -> String -> (String -> msg) -> ( State query liveQuery msg, Cmd msg )
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
    ( newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


query : State query liveQuery msg -> Lantern.Query.Query -> Json.Decode.Decoder query -> (Result Error query -> msg) -> ( State query liveQuery msg, Cmd msg )
query ({ requestsInFlight } as state) query_ decoder msg =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.ReaderQuery query_

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
    ( newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


liveQuery :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> (List a -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery ( queryA, decoderA ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA ] ->
                    Result.map resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA ] liveResultHandler state


liveQuery2 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> (List a -> List b -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery2 ( queryA, decoderA ) ( queryB, decoderB ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB ] ->
                    Result.map2 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB ] liveResultHandler state


liveQuery3 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> (List a -> List b -> List c -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery3 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC ] ->
                    Result.map3 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC ] liveResultHandler state


liveQuery4 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> (List a -> List b -> List c -> List d -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery4 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD ] ->
                    Result.map4 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD ] liveResultHandler state


liveQuery5 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> (List a -> List b -> List c -> List d -> List e -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery5 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE ] ->
                    Result.map5 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE ] liveResultHandler state


liveQuery6 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> ( Lantern.Query.Query, Json.Decode.Decoder f )
    -> (List a -> List b -> List c -> List d -> List e -> List f -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery6 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) ( queryF, decoderF ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE, resultF ] ->
                    Lantern.Extra.Result.map6 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderF) resultF |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE, queryF ] liveResultHandler state


liveQuery7 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> ( Lantern.Query.Query, Json.Decode.Decoder f )
    -> ( Lantern.Query.Query, Json.Decode.Decoder g )
    -> (List a -> List b -> List c -> List d -> List e -> List f -> List g -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery7 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) ( queryF, decoderF ) ( queryG, decoderG ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE, resultF, resultG ] ->
                    Lantern.Extra.Result.map7 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderF) resultF |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderG) resultG |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE, queryF, queryG ] liveResultHandler state


liveQuery8 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> ( Lantern.Query.Query, Json.Decode.Decoder f )
    -> ( Lantern.Query.Query, Json.Decode.Decoder g )
    -> ( Lantern.Query.Query, Json.Decode.Decoder h )
    -> (List a -> List b -> List c -> List d -> List e -> List f -> List g -> List h -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery8 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) ( queryF, decoderF ) ( queryG, decoderG ) ( queryH, decoderH ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE, resultF, resultG, resultH ] ->
                    Lantern.Extra.Result.map8 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderF) resultF |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderG) resultG |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderH) resultH |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE, queryF, queryG, queryH ] liveResultHandler state


liveQuery9 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> ( Lantern.Query.Query, Json.Decode.Decoder f )
    -> ( Lantern.Query.Query, Json.Decode.Decoder g )
    -> ( Lantern.Query.Query, Json.Decode.Decoder h )
    -> ( Lantern.Query.Query, Json.Decode.Decoder i )
    -> (List a -> List b -> List c -> List d -> List e -> List f -> List g -> List h -> List i -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery9 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) ( queryF, decoderF ) ( queryG, decoderG ) ( queryH, decoderH ) ( queryI, decoderI ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE, resultF, resultG, resultH, resultI ] ->
                    Lantern.Extra.Result.map9 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderF) resultF |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderG) resultG |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderH) resultH |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderI) resultI |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE, queryF, queryG, queryH, queryI ] liveResultHandler state


liveQuery10 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> ( Lantern.Query.Query, Json.Decode.Decoder d )
    -> ( Lantern.Query.Query, Json.Decode.Decoder e )
    -> ( Lantern.Query.Query, Json.Decode.Decoder f )
    -> ( Lantern.Query.Query, Json.Decode.Decoder g )
    -> ( Lantern.Query.Query, Json.Decode.Decoder h )
    -> ( Lantern.Query.Query, Json.Decode.Decoder i )
    -> ( Lantern.Query.Query, Json.Decode.Decoder j )
    -> (List a -> List b -> List c -> List d -> List e -> List f -> List g -> List h -> List i -> List j -> liveQuery)
    -> (Result Error liveQuery -> msg)
    -> State query liveQuery msg
    -> ( State query liveQuery msg, Cmd msg )
liveQuery10 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) ( queryD, decoderD ) ( queryE, decoderE ) ( queryF, decoderF ) ( queryG, decoderG ) ( queryH, decoderH ) ( queryI, decoderI ) ( queryJ, decoderJ ) resultConstructor msg state =
    let
        decodeResults results =
            case results of
                [ resultA, resultB, resultC, resultD, resultE, resultF, resultG, resultH, resultI, resultJ ] ->
                    Lantern.Extra.Result.map10 resultConstructor
                        (Json.Decode.decodeValue (Json.Decode.list decoderA) resultA |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderB) resultB |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderC) resultC |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderD) resultD |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderE) resultE |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderF) resultF |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderG) resultG |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderH) resultH |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderI) resultI |> Result.mapError (Json.Decode.errorToString >> Error))
                        (Json.Decode.decodeValue (Json.Decode.list decoderJ) resultJ |> Result.mapError (Json.Decode.errorToString >> Error))

                _ ->
                    Err (Error ("unexpected number of liveQuery results: " ++ String.fromInt (List.length results)))

        liveResultHandler =
            { decodeResults = decodeResults, msg = msg }
    in
    liveQuery_ [ queryA, queryB, queryC, queryD, queryE, queryF, queryG, queryH, queryI, queryJ ] liveResultHandler state


liveQuery_ : List Lantern.Query.Query -> LiveResultHandler liveQuery msg -> State query liveQuery msg -> ( State query liveQuery msg, Cmd msg )
liveQuery_ queries liveResultHandler state =
    let
        requestId =
            "LiveQuery"

        request =
            Lantern.Request.LiveQuery queries

        newState =
            { state
                | liveResultHandler = Just liveResultHandler
                , log = Log.logRequest state.log requestId request
            }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Encoders.request requestId request)) )


migrate : Lantern.Query.Query -> (Bool -> msg) -> State query liveQuery msg -> ( State query liveQuery msg, Cmd msg )
migrate query_ msg ({ requestsInFlight } as state) =
    let
        requestsCounter =
            state.requestId + 1

        requestId =
            String.fromInt requestsCounter

        request =
            Lantern.Request.Migration query_

        newRequestsInFlight =
            { requestsInFlight | migration = Dict.insert requestId msg requestsInFlight.migration }

        newState =
            { state
                | requestId = requestsCounter
                , requestsInFlight = newRequestsInFlight
                , log = Log.logRequest state.log requestId request
            }
    in
    ( newState, state.requestPort (Json.Encode.encode 0 (Encoders.request (String.fromInt newState.requestId) request)) )


update : Message -> State query liveQuery msg -> ( State query liveQuery msg, Cmd msg )
update msg ({ requestsInFlight } as state) =
    case msg of
        ResponseMsg payload ->
            let
                parsedResponse =
                    Json.Decode.decodeString Decoders.response payload
            in
            case parsedResponse of
                Ok ( id, response ) ->
                    case response of
                        Lantern.Response.Echo text ->
                            let
                                handler =
                                    Dict.get id requestsInFlight.echo

                                newRequestsInFlight =
                                    { requestsInFlight | echo = Dict.remove id requestsInFlight.echo }

                                newState =
                                    { state
                                        | requestsInFlight = newRequestsInFlight
                                        , log = Log.logResponse state.log id response
                                    }
                            in
                            case handler of
                                Just callback ->
                                    ( newState, Task.perform callback (Task.succeed text) )

                                Nothing ->
                                    ( newState, Cmd.none )

                        Lantern.Response.ReaderQuery results ->
                            let
                                handler =
                                    Dict.get id requestsInFlight.query

                                newRequestsInFlight =
                                    { requestsInFlight | query = Dict.remove id requestsInFlight.query }

                                newState =
                                    { state
                                        | requestsInFlight = newRequestsInFlight
                                        , log = Log.logResponse state.log id response
                                    }
                            in
                            case handler of
                                Just ( decoder, callback ) ->
                                    let
                                        parseResult =
                                            Lantern.Query.decodeResult results decoder
                                                |> Result.mapError (Json.Decode.errorToString >> Error)
                                    in
                                    ( newState, Task.perform callback (Task.succeed parseResult) )

                                Nothing ->
                                    ( newState, Cmd.none )

                        Lantern.Response.WriterQuery result ->
                            Debug.todo "Handle writer results"

                        Lantern.Response.LiveQuery results ->
                            let
                                cmd =
                                    state.liveResultHandler
                                        |> Maybe.map
                                            (\handler ->
                                                Task.perform handler.msg (Task.succeed (handler.decodeResults results))
                                            )
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( state, cmd )

                        Lantern.Response.Migration ->
                            let
                                handler =
                                    Dict.get id requestsInFlight.migration

                                newRequestsInFlight =
                                    { requestsInFlight | migration = Dict.remove id requestsInFlight.migration }

                                newState =
                                    { state
                                        | requestsInFlight = newRequestsInFlight
                                        , log = Log.logResponse state.log id Lantern.Response.Migration
                                    }
                            in
                            case handler of
                                Just callback ->
                                    ( newState, Task.perform callback (Task.succeed True) )

                                Nothing ->
                                    ( newState, Cmd.none )

                        Lantern.Response.Unknown _ ->
                            ( { state | log = Log.logResponse state.log id response }, Cmd.none )

                Err err ->
                    ( { state | log = Log.log state.log Log.Error (Json.Decode.errorToString err) }, Cmd.none )


errorToString : Error -> String
errorToString (Error e) =
    e
