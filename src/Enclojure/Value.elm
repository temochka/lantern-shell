module Enclojure.Value exposing
    ( exception
    , inspect
    , inspectLocated
    , toMap
    , toSeq
    , toString
    , tryDictOf
    , tryKeyword
    , tryMap
    , trySequenceOf
    , tryString
    , trySymbol
    )

import Array
import Dict
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Types exposing (Env, Exception(..), Number(..), Value(..))
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet


toSeq : Value io -> Result Exception (List (Located (Value io)))
toSeq val =
    case val of
        List l ->
            Ok l

        Vector v ->
            Ok <| Array.toList v

        Set s ->
            Ok <| List.map Located.unknown <| ValueSet.toList s

        Map m ->
            Ok <| List.map (\(( _, Located loc _ ) as entry) -> Located loc (MapEntry entry)) (ValueMap.toList m)

        String s ->
            Ok <| List.map (String.fromChar >> String >> Located.unknown) (String.toList s)

        MapEntry ( k, v ) ->
            Ok <| [ Located.sameAs v k, v ]

        Nil ->
            Ok []

        _ ->
            Err <| Exception (inspect val ++ " is not a sequence") []


toMap : Value io -> Maybe (ValueMap io)
toMap val =
    case val of
        Nil ->
            Just ValueMap.empty

        Vector vec ->
            Array.toList vec |> List.indexedMap (\i e -> ( Number <| Int i, e )) |> ValueMap.fromList |> Just

        Map m ->
            Just m

        _ ->
            Nothing


tryString : Value io -> Maybe String
tryString value =
    case value of
        String s ->
            Just s

        _ ->
            Nothing


tryKeyword : Value io -> Maybe String
tryKeyword value =
    case value of
        Keyword s ->
            Just s

        _ ->
            Nothing


trySymbol : Value io -> Maybe String
trySymbol value =
    case value of
        Symbol s ->
            Just s

        _ ->
            Nothing


tryMap : Value io -> Maybe (ValueMap io)
tryMap value =
    case value of
        Map s ->
            Just s

        _ ->
            Nothing


tryDictOf : (Value io -> Maybe comparable) -> (Value io -> Maybe b) -> Value io -> Maybe (Dict.Dict comparable b)
tryDictOf extractKey extractValue value =
    let
        extractAll kvSequence =
            kvSequence
                |> List.foldr
                    (\( key, val ) a ->
                        a
                            |> Maybe.andThen
                                (\acc ->
                                    Maybe.map2
                                        (\extractedKey extractedVal -> ( extractedKey, extractedVal ) :: acc)
                                        (extractKey key)
                                        (extractValue (Located.getValue val))
                                )
                    )
                    (Just [])
    in
    case value of
        Map m ->
            m |> ValueMap.toList |> extractAll |> Maybe.map Dict.fromList

        _ ->
            Nothing


trySequenceOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
trySequenceOf extract value =
    let
        extractAll sequence =
            sequence
                |> List.foldr
                    (\e a ->
                        a
                            |> Maybe.andThen
                                (\acc ->
                                    extract e
                                        |> Maybe.map (\extracted -> extracted :: acc)
                                )
                    )
                    (Just [])
    in
    toSeq value
        |> Result.map (List.map Located.getValue)
        |> Result.toMaybe
        |> Maybe.andThen extractAll


exception : Env io -> String -> Exception
exception env message =
    Exception message env.stack


inspectLocated : Located (Value io) -> String
inspectLocated locatedValue =
    let
        suffix =
            locatedValue
                |> Located.getOffsets
                |> Maybe.map (\{ start } -> ":" ++ String.fromInt (Tuple.first start) ++ ":" ++ String.fromInt (Tuple.second start))
                |> Maybe.withDefault ""
    in
    inspect (Located.getValue locatedValue) ++ suffix


inspect : Value io -> String
inspect value =
    case value of
        Ref name _ ->
            "#'" ++ name

        String string ->
            "\"" ++ string ++ "\""

        Number (Int x) ->
            String.fromInt x

        Number (Float x) ->
            String.fromFloat x

        Fn name _ ->
            "fn<" ++ (name |> Maybe.withDefault "anonymous") ++ ">"

        List l ->
            "(" ++ (List.map (Located.getValue >> inspect) l |> String.join " ") ++ ")"

        Nil ->
            "nil"

        Bool b ->
            if b then
                "true"

            else
                "false"

        Vector l ->
            "[" ++ (List.map (Located.getValue >> inspect) (Array.toList l) |> String.join " ") ++ "]"

        Keyword name ->
            ":" ++ name

        Map m ->
            List.map (\( k, Located _ v ) -> inspect k ++ " " ++ inspect v) (ValueMap.toList m)
                |> String.join ", "
                |> (\r -> "{" ++ r ++ "}")

        MapEntry ( k, v ) ->
            inspect (Vector (Array.fromList [ Located.unknown k, v ]))

        Set set ->
            List.map (\v -> inspect v) (ValueSet.toList set)
                |> String.join ", "
                |> (\r -> "#{" ++ r ++ "}")

        Symbol name ->
            name

        Throwable (Exception str _) ->
            "Exception: " ++ str


print : Value io -> String
print value =
    case value of
        String string ->
            string

        Nil ->
            ""

        _ ->
            inspect value


toString : Value io -> String
toString value =
    case value of
        String s ->
            s

        _ ->
            print value
