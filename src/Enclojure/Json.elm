module Enclojure.Json exposing (decodeFromString, encodeToString)

import Array
import Dict exposing (Dict)
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Exception(..), Number(..), Value(..), ValueMap)
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Json.Decode
import Json.Encode


decode : Json.Decode.Decoder Value
decode =
    Json.Decode.oneOf
        [ Json.Decode.string |> Json.Decode.map String
        , Json.Decode.bool |> Json.Decode.map Bool
        , Json.Decode.int |> Json.Decode.map (Int >> Number)
        , Json.Decode.float |> Json.Decode.map (Float >> Number)
        , Json.Decode.null Nil
        , Json.Decode.array (Json.Decode.lazy (\_ -> decode)) |> Json.Decode.map (Array.map Located.unknown >> Vector)
        , Json.Decode.dict (Json.Decode.lazy (\_ -> decode))
            |> Json.Decode.map
                (Dict.toList
                    >> List.map (\( k, v ) -> ( String k, Located.unknown v ))
                    >> ValueMap.fromList
                    >> Map
                )
        ]


decodeFromString : String -> Result Exception Value
decodeFromString json =
    Json.Decode.decodeString decode json
        |> Result.mapError (Json.Decode.errorToString >> String.append "JSON parsing error: " >> Exception)


toDict : ValueMap -> Dict String Json.Encode.Value
toDict map =
    map
        |> ValueMap.toList
        |> List.filterMap
            (\( k, Located _ v ) ->
                (case k of
                    String s ->
                        Just s

                    Keyword s ->
                        Just s

                    _ ->
                        Nothing
                )
                    |> Maybe.map (\stringKey -> ( stringKey, encode v ))
            )
        |> Dict.fromList


encode : Value -> Json.Encode.Value
encode val =
    case val of
        Number n ->
            case n of
                Int i ->
                    Json.Encode.int i

                Float f ->
                    Json.Encode.float f

        String s ->
            Json.Encode.string s

        Ref _ (Located _ v) ->
            encode v

        Fn _ _ ->
            Json.Encode.null

        List l ->
            Json.Encode.list encode (List.map Located.getValue l)

        Vector a ->
            Json.Encode.array encode (Array.map Located.getValue a)

        Nil ->
            Json.Encode.null

        Bool b ->
            Json.Encode.bool b

        Keyword s ->
            Json.Encode.string s

        Map vm ->
            Json.Encode.dict identity identity (toDict vm)

        MapEntry ( k, Located _ v ) ->
            Json.Encode.list encode [ k, v ]

        Set vs ->
            Json.Encode.list encode (ValueSet.toList vs)

        Symbol s ->
            Json.Encode.string s

        Throwable _ ->
            Json.Encode.null


encodeToString : Value -> String
encodeToString val =
    Json.Encode.encode 0 (encode val)
