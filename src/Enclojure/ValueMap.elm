module Enclojure.ValueMap exposing (empty, foldl, fromList, get, insert, isEmpty, map, member, remove, toList)

import Dict
import Enclojure.Located exposing (Located(..))
import Enclojure.Types exposing (..)


empty : ValueMap io
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
    , throwables = []
    , vectors = []
    , keywords = Dict.empty
    }


isEmpty : ValueMap io -> Bool
isEmpty m =
    Dict.isEmpty m.ints
        && Dict.isEmpty m.floats
        && Dict.isEmpty m.strings
        && List.isEmpty m.refs
        && m.nil
        == Nothing
        && m.bools
        == { true = Nothing, false = Nothing }
        && Dict.isEmpty m.symbols
        && List.isEmpty m.fns
        && List.isEmpty m.maps
        && List.isEmpty m.mapEntries
        && List.isEmpty m.lists
        && List.isEmpty m.sets
        && List.isEmpty m.vectors
        && Dict.isEmpty m.keywords


insert : Value io -> Located (Value io) -> ValueMap io -> ValueMap io
insert k v m =
    case k of
        Number (Int int) ->
            { m | ints = Dict.insert int v m.ints }

        Number (Float float) ->
            { m | floats = Dict.insert float v m.floats }

        String string ->
            { m | strings = Dict.insert string v m.strings }

        Ref _ _ ->
            { m | refs = ( k, v ) :: m.refs }

        List _ ->
            { m | lists = ( k, v ) :: m.lists }

        Nil ->
            { m | nil = Just v }

        Bool bool ->
            let
                oldBools =
                    m.bools

                bools =
                    if bool then
                        { oldBools | true = Just v }

                    else
                        { oldBools | false = Just v }
            in
            { m | bools = bools }

        Keyword keyword ->
            { m | keywords = Dict.insert keyword v m.keywords }

        Symbol symbol ->
            { m | symbols = Dict.insert symbol v m.symbols }

        Fn _ _ ->
            { m | fns = ( k, v ) :: m.fns }

        Map _ ->
            { m | maps = ( k, v ) :: m.maps }

        MapEntry _ ->
            { m | mapEntries = ( k, v ) :: m.mapEntries }

        Set _ ->
            { m | sets = ( k, v ) :: m.sets }

        Throwable _ ->
            { m | throwables = ( k, v ) :: m.throwables }

        Vector _ ->
            { m | vectors = ( k, v ) :: m.vectors }


remove : Value io -> ValueMap io -> ValueMap io
remove k m =
    case k of
        Number (Int int) ->
            { m | ints = Dict.remove int m.ints }

        Number (Float float) ->
            { m | floats = Dict.remove float m.floats }

        String string ->
            { m | strings = Dict.remove string m.strings }

        Ref _ _ ->
            let
                newRefs =
                    m.refs |> List.filter (Tuple.first >> (/=) k)
            in
            { m | refs = newRefs }

        List _ ->
            let
                newLists =
                    m.lists |> List.filter (Tuple.first >> (/=) k)
            in
            { m | lists = newLists }

        Nil ->
            { m | nil = Nothing }

        Bool bool ->
            let
                oldBools =
                    m.bools

                bools =
                    if bool then
                        { oldBools | true = Nothing }

                    else
                        { oldBools | false = Nothing }
            in
            { m | bools = bools }

        Keyword keyword ->
            { m | keywords = Dict.remove keyword m.keywords }

        Symbol symbol ->
            { m | symbols = Dict.remove symbol m.symbols }

        Fn _ _ ->
            let
                newFns =
                    m.fns |> List.filter (Tuple.first >> (/=) k)
            in
            { m | fns = newFns }

        Map _ ->
            let
                newMaps =
                    m.maps |> List.filter (Tuple.first >> (/=) k)
            in
            { m | maps = newMaps }

        MapEntry _ ->
            let
                newMapEntries =
                    m.mapEntries |> List.filter (Tuple.first >> (/=) k)
            in
            { m | mapEntries = newMapEntries }

        Set _ ->
            let
                newSets =
                    m.sets |> List.filter (Tuple.first >> (/=) k)
            in
            { m | sets = newSets }

        Throwable _ ->
            m

        Vector _ ->
            let
                newVectors =
                    m.vectors |> List.filter (Tuple.first >> (/=) k)
            in
            { m | vectors = newVectors }


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


get : Value io -> ValueMap io -> Maybe (Located (Value io))
get k m =
    case k of
        Number (Int int) ->
            Dict.get int m.ints

        Number (Float float) ->
            Dict.get float m.floats

        String string ->
            Dict.get string m.strings

        Ref _ _ ->
            linearFind (Tuple.first >> (==) k) m.refs
                |> Maybe.map Tuple.second

        List _ ->
            linearFind (Tuple.first >> (==) k) m.lists
                |> Maybe.map Tuple.second

        Nil ->
            m.nil

        Bool bool ->
            if bool then
                m.bools.true

            else
                m.bools.false

        Keyword keyword ->
            Dict.get keyword m.keywords

        Symbol symbol ->
            Dict.get symbol m.symbols

        Fn _ _ ->
            linearFind (Tuple.first >> (==) k) m.fns
                |> Maybe.map Tuple.second

        Map _ ->
            linearFind (Tuple.first >> (==) k) m.maps
                |> Maybe.map Tuple.second

        MapEntry _ ->
            linearFind (Tuple.first >> (==) k) m.mapEntries
                |> Maybe.map Tuple.second

        Set _ ->
            linearFind (Tuple.first >> (==) k) m.sets
                |> Maybe.map Tuple.second

        Throwable _ ->
            Nothing

        Vector _ ->
            linearFind (Tuple.first >> (==) k) m.vectors
                |> Maybe.map Tuple.second


member : Value io -> ValueMap io -> Bool
member val m =
    Nothing /= get val m


toList : ValueMap io -> List ( Value io, Located (Value io) )
toList m =
    let
        ints =
            Dict.toList m.ints |> List.map (Tuple.mapFirst (Int >> Number))

        floats =
            Dict.toList m.floats |> List.map (Tuple.mapFirst (Float >> Number))

        strings =
            Dict.toList m.strings |> List.map (Tuple.mapFirst String)

        nils =
            m.nil |> Maybe.map (Tuple.pair Nil >> List.singleton) |> Maybe.withDefault []

        trues =
            m.bools.true |> Maybe.map (Tuple.pair (Bool True) >> List.singleton) |> Maybe.withDefault []

        falses =
            m.bools.false |> Maybe.map (Tuple.pair (Bool False) >> List.singleton) |> Maybe.withDefault []

        keywords =
            Dict.toList m.keywords |> List.map (Tuple.mapFirst Keyword)

        symbols =
            Dict.toList m.symbols |> List.map (Tuple.mapFirst Symbol)
    in
    ints
        ++ floats
        ++ strings
        ++ nils
        ++ trues
        ++ falses
        ++ keywords
        ++ symbols
        ++ m.refs
        ++ m.fns
        ++ m.maps
        ++ m.mapEntries
        ++ m.lists
        ++ m.sets
        ++ m.vectors


foldl : (Value io -> Located (Value io) -> a -> a) -> a -> ValueMap io -> a
foldl fn init m =
    List.foldl (\( k, v ) a -> fn k v a) init (toList m)


fromList : List (ValueMapEntry io) -> ValueMap io
fromList entries =
    entries
        |> List.foldl (\( k, v ) a -> insert k v a) empty


map : (ValueMapEntry io -> ValueMapEntry io) -> ValueMap io -> ValueMap io
map f m =
    m |> toList |> List.map f |> fromList
