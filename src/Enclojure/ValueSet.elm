module Enclojure.ValueSet exposing (ValueSet, empty, fromList, insert, isEmpty, map, member, remove, toList)

import Enclojure.Common exposing (Number(..), Value(..))
import Set


type alias ValueSet io =
    Enclojure.Common.ValueSet io


empty : ValueSet io
empty =
    Enclojure.Common.ValueSet
        { ints = Set.empty
        , floats = Set.empty
        , strings = Set.empty
        , refs = []
        , nil = Nothing
        , bools = { true = False, false = False }
        , symbols = Set.empty
        , fns = []
        , maps = []
        , mapEntries = []
        , lists = []
        , regexs = []
        , vectors = []
        , keywords = Set.empty
        , sets = []
        , throwables = []
        }


isEmpty : ValueSet io -> Bool
isEmpty (Enclojure.Common.ValueSet m) =
    Set.isEmpty m.ints
        && Set.isEmpty m.floats
        && Set.isEmpty m.strings
        && List.isEmpty m.refs
        && m.nil
        == Nothing
        && m.bools
        == { true = False, false = False }
        && Set.isEmpty m.symbols
        && List.isEmpty m.fns
        && List.isEmpty m.maps
        && List.isEmpty m.mapEntries
        && List.isEmpty m.lists
        && List.isEmpty m.sets
        && List.isEmpty m.vectors
        && Set.isEmpty m.keywords


insert : Value io -> ValueSet io -> ValueSet io
insert v (Enclojure.Common.ValueSet set) =
    Enclojure.Common.ValueSet <|
        case v of
            Number (Int int) ->
                { set | ints = Set.insert int set.ints }

            Number (Float float) ->
                { set | floats = Set.insert float set.floats }

            String string ->
                { set | strings = Set.insert string set.strings }

            Ref _ ->
                { set | refs = v :: set.refs }

            List _ ->
                { set | lists = v :: set.lists }

            Nil ->
                { set | nil = Just v }

            Bool bool ->
                let
                    oldBools =
                        set.bools

                    bools =
                        if bool then
                            { oldBools | true = True }

                        else
                            { oldBools | false = True }
                in
                { set | bools = bools }

            Keyword keyword ->
                { set | keywords = Set.insert keyword set.keywords }

            Symbol symbol ->
                { set | symbols = Set.insert symbol set.symbols }

            Fn _ _ ->
                { set | fns = v :: set.fns }

            Map _ ->
                { set | maps = v :: set.maps }

            MapEntry _ ->
                { set | mapEntries = v :: set.mapEntries }

            Regex _ _ ->
                { set | regexs = v :: set.regexs }

            Set _ ->
                { set | sets = v :: set.sets }

            Throwable _ ->
                { set | throwables = v :: set.throwables }

            Vector _ ->
                { set | vectors = v :: set.vectors }


remove : Value io -> ValueSet io -> ValueSet io
remove v (Enclojure.Common.ValueSet set) =
    Enclojure.Common.ValueSet <|
        case v of
            Number (Int int) ->
                { set | ints = Set.remove int set.ints }

            Number (Float float) ->
                { set | floats = Set.remove float set.floats }

            String string ->
                { set | strings = Set.remove string set.strings }

            Ref _ ->
                let
                    newRefs =
                        set.refs |> List.filter ((/=) v)
                in
                { set | refs = newRefs }

            List _ ->
                let
                    newLists =
                        set.lists |> List.filter ((/=) v)
                in
                { set | lists = newLists }

            Nil ->
                { set | nil = Nothing }

            Bool bool ->
                let
                    oldBools =
                        set.bools

                    bools =
                        if bool then
                            { oldBools | true = False }

                        else
                            { oldBools | false = False }
                in
                { set | bools = bools }

            Keyword keyword ->
                { set | keywords = Set.remove keyword set.keywords }

            Symbol symbol ->
                { set | symbols = Set.remove symbol set.symbols }

            Fn _ _ ->
                let
                    newFns =
                        set.fns |> List.filter ((/=) v)
                in
                { set | fns = newFns }

            Map _ ->
                let
                    newMaps =
                        set.maps |> List.filter ((/=) v)
                in
                { set | maps = newMaps }

            MapEntry _ ->
                let
                    newMapEntries =
                        set.mapEntries |> List.filter ((/=) v)
                in
                { set | mapEntries = newMapEntries }

            Regex _ _ ->
                let
                    newRegexs =
                        set.regexs |> List.filter ((/=) v)
                in
                { set | regexs = newRegexs }

            Set _ ->
                let
                    newSets =
                        set.sets |> List.filter ((/=) v)
                in
                { set | sets = newSets }

            Throwable _ ->
                set

            Vector _ ->
                let
                    newVectors =
                        set.vectors |> List.filter ((/=) v)
                in
                { set | vectors = newVectors }


fromList : List (Value io) -> ValueSet io
fromList entries =
    entries
        |> List.foldl (\v a -> insert v a) empty


toList : ValueSet io -> List (Value io)
toList (Enclojure.Common.ValueSet set) =
    let
        ints =
            Set.toList set.ints |> List.map (Int >> Number)

        floats =
            Set.toList set.floats |> List.map (Float >> Number)

        strings =
            Set.toList set.strings |> List.map String

        nils =
            set.nil |> Maybe.map List.singleton |> Maybe.withDefault []

        trues =
            if set.bools.true then
                [ Bool True ]

            else
                []

        falses =
            if set.bools.false then
                [ Bool False ]

            else
                []

        keywords =
            Set.toList set.keywords |> List.map Keyword

        symbols =
            Set.toList set.symbols |> List.map Symbol
    in
    ints
        ++ floats
        ++ strings
        ++ nils
        ++ trues
        ++ falses
        ++ keywords
        ++ symbols
        ++ set.refs
        ++ set.fns
        ++ set.maps
        ++ set.mapEntries
        ++ set.lists
        ++ set.sets
        ++ set.regexs
        ++ set.throwables
        ++ set.vectors


map : (Value io -> Value io) -> ValueSet io -> ValueSet io
map f set =
    set |> toList |> List.map f |> fromList


member : Value io -> ValueSet io -> Bool
member v (Enclojure.Common.ValueSet set) =
    case v of
        Number (Int int) ->
            Set.member int set.ints

        Number (Float float) ->
            Set.member float set.floats

        String string ->
            Set.member string set.strings

        Ref _ ->
            False

        List _ ->
            List.member v set.lists

        Nil ->
            set.nil /= Nothing

        Bool bool ->
            if bool then
                set.bools.true

            else
                set.bools.false

        Keyword keyword ->
            Set.member keyword set.keywords

        Symbol symbol ->
            Set.member symbol set.symbols

        Fn _ _ ->
            False

        Map _ ->
            List.member v set.maps

        MapEntry _ ->
            List.member v set.mapEntries

        Regex _ _ ->
            List.member v set.regexs

        Set _ ->
            List.member v set.sets

        Throwable _ ->
            False

        Vector _ ->
            List.member v set.vectors
