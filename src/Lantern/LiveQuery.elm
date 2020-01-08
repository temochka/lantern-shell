module Lantern.LiveQuery exposing (LiveQuery(..), areEqual, areEqualLists, map, prepare, prepare2, prepare3)

import Json.Decode
import Lantern.Errors exposing (Error(..))
import Lantern.Query


type LiveQuery msg
    = LiveQuery (List Lantern.Query.Query) (List Lantern.Query.ReaderResult -> msg)


areEqual : LiveQuery msg -> LiveQuery msg -> Bool
areEqual (LiveQuery a _) (LiveQuery b _) =
    a == b


areEqualLists : List (LiveQuery msg) -> List (LiveQuery msg) -> Bool
areEqualLists liveQueriesA liveQueriesB =
    case ( liveQueriesA, liveQueriesB ) of
        ( liveQueryA :: restA, liveQueryB :: restB ) ->
            if areEqual liveQueryA liveQueryB then
                areEqualLists restA restB

            else
                False

        ( [], [] ) ->
            True

        _ ->
            False


map : (a -> b) -> LiveQuery a -> LiveQuery b
map mapf (LiveQuery queries handler) =
    LiveQuery queries (handler >> mapf)


prepare :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> (Result Error (List a) -> msg)
    -> LiveQuery msg
prepare ( queryA, decoderA ) msg =
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


prepare2 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> (Result Error ( List a, List b ) -> msg)
    -> LiveQuery msg
prepare2 ( queryA, decoderA ) ( queryB, decoderB ) msg =
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


prepare3 :
    ( Lantern.Query.Query, Json.Decode.Decoder a )
    -> ( Lantern.Query.Query, Json.Decode.Decoder b )
    -> ( Lantern.Query.Query, Json.Decode.Decoder c )
    -> (Result Error ( List a, List b, List c ) -> msg)
    -> LiveQuery msg
prepare3 ( queryA, decoderA ) ( queryB, decoderB ) ( queryC, decoderC ) msg =
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
