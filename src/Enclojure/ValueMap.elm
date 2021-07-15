module Enclojure.ValueMap exposing (empty, fromList, get, insert, remove, toList)

import Dict
import Enclojure.Located exposing (Located(..))
import Enclojure.Types exposing (..)


empty : ValueMap
empty =
    { ints = Dict.empty
    , floats = Dict.empty
    , strings = Dict.empty
    , refs = []
    , nil = Nothing
    , bools = { true = Nothing, false = Nothing }
    , symbols = Dict.empty
    , fns = []
    , maps = []
    , mapEntries = []
    , lists = []
    , sets = []
    , vectors = []
    , keywords = Dict.empty
    }


insert : Value -> Located Value -> ValueMap -> ValueMap
insert k v map =
    case k of
        Int int ->
            { map | ints = Dict.insert int v map.ints }

        Float float ->
            { map | floats = Dict.insert float v map.floats }

        String string ->
            { map | strings = Dict.insert string v map.strings }

        Ref _ _ ->
            { map | refs = ( k, v ) :: map.refs }

        List _ ->
            { map | lists = ( k, v ) :: map.lists }

        Nil ->
            { map | nil = Just v }

        Bool bool ->
            let
                oldBools =
                    map.bools

                bools =
                    if bool then
                        { oldBools | true = Just v }

                    else
                        { oldBools | false = Just v }
            in
            { map | bools = bools }

        Keyword keyword ->
            { map | keywords = Dict.insert keyword v map.keywords }

        Symbol symbol ->
            { map | symbols = Dict.insert symbol v map.symbols }

        Fn _ _ ->
            { map | fns = ( k, v ) :: map.fns }

        Map _ ->
            { map | maps = ( k, v ) :: map.maps }

        MapEntry _ ->
            { map | mapEntries = ( k, v ) :: map.mapEntries }

        Set _ ->
            { map | sets = ( k, v ) :: map.sets }

        Vector _ ->
            { map | vectors = ( k, v ) :: map.vectors }


remove : Value -> ValueMap -> ValueMap
remove k map =
    case k of
        Int int ->
            { map | ints = Dict.remove int map.ints }

        Float float ->
            { map | floats = Dict.remove float map.floats }

        String string ->
            { map | strings = Dict.remove string map.strings }

        Ref _ _ ->
            let
                newRefs =
                    map.refs |> List.filter (Tuple.first >> (/=) k)
            in
            { map | refs = newRefs }

        List _ ->
            let
                newLists =
                    map.lists |> List.filter (Tuple.first >> (/=) k)
            in
            { map | lists = newLists }

        Nil ->
            { map | nil = Nothing }

        Bool bool ->
            let
                oldBools =
                    map.bools

                bools =
                    if bool then
                        { oldBools | true = Nothing }

                    else
                        { oldBools | false = Nothing }
            in
            { map | bools = bools }

        Keyword keyword ->
            { map | keywords = Dict.remove keyword map.keywords }

        Symbol symbol ->
            { map | symbols = Dict.remove symbol map.symbols }

        Fn _ _ ->
            let
                newFns =
                    map.fns |> List.filter (Tuple.first >> (/=) k)
            in
            { map | fns = newFns }

        Map _ ->
            let
                newMaps =
                    map.maps |> List.filter (Tuple.first >> (/=) k)
            in
            { map | maps = newMaps }

        MapEntry _ ->
            let
                newMapEntries =
                    map.mapEntries |> List.filter (Tuple.first >> (/=) k)
            in
            { map | mapEntries = newMapEntries }

        Set _ ->
            let
                newSets =
                    map.sets |> List.filter (Tuple.first >> (/=) k)
            in
            { map | sets = newSets }

        Vector _ ->
            let
                newVectors =
                    map.vectors |> List.filter (Tuple.first >> (/=) k)
            in
            { map | vectors = newVectors }


linearFind : (a -> Bool) -> List a -> Maybe a
linearFind f l =
    case l of
        [] ->
            Nothing

        a :: rest ->
            if f a then
                Just a

            else
                linearFind f rest


get : Value -> ValueMap -> Maybe (Located Value)
get k map =
    case k of
        Int int ->
            Dict.get int map.ints

        Float float ->
            Dict.get float map.floats

        String string ->
            Dict.get string map.strings

        Ref _ _ ->
            linearFind (Tuple.first >> (==) k) map.refs
                |> Maybe.map Tuple.second

        List _ ->
            linearFind (Tuple.first >> (==) k) map.lists
                |> Maybe.map Tuple.second

        Nil ->
            map.nil

        Bool bool ->
            if bool then
                map.bools.true

            else
                map.bools.false

        Keyword keyword ->
            Dict.get keyword map.keywords

        Symbol symbol ->
            Dict.get symbol map.symbols

        Fn _ _ ->
            linearFind (Tuple.first >> (==) k) map.fns
                |> Maybe.map Tuple.second

        Map _ ->
            linearFind (Tuple.first >> (==) k) map.maps
                |> Maybe.map Tuple.second

        MapEntry _ ->
            linearFind (Tuple.first >> (==) k) map.mapEntries
                |> Maybe.map Tuple.second

        Set _ ->
            linearFind (Tuple.first >> (==) k) map.sets
                |> Maybe.map Tuple.second

        Vector _ ->
            linearFind (Tuple.first >> (==) k) map.vectors
                |> Maybe.map Tuple.second


toList : ValueMap -> List ( Value, Located Value )
toList map =
    let
        ints =
            Dict.toList map.ints |> List.map (Tuple.mapFirst Int)

        floats =
            Dict.toList map.floats |> List.map (Tuple.mapFirst Float)

        strings =
            Dict.toList map.strings |> List.map (Tuple.mapFirst String)

        nils =
            map.nil |> Maybe.map (Tuple.pair Nil >> List.singleton) |> Maybe.withDefault []

        trues =
            map.bools.true |> Maybe.map (Tuple.pair (Bool True) >> List.singleton) |> Maybe.withDefault []

        falses =
            map.bools.false |> Maybe.map (Tuple.pair (Bool False) >> List.singleton) |> Maybe.withDefault []

        keywords =
            Dict.toList map.keywords |> List.map (Tuple.mapFirst Keyword)

        symbols =
            Dict.toList map.symbols |> List.map (Tuple.mapFirst Symbol)
    in
    ints ++ floats ++ strings ++ nils ++ trues ++ falses ++ keywords ++ symbols ++ map.refs ++ map.fns ++ map.maps ++ map.mapEntries ++ map.lists ++ map.sets ++ map.vectors


fromList : List ValueMapEntry -> ValueMap
fromList entries =
    entries
        |> List.foldl (\( k, v ) a -> insert k v a) empty