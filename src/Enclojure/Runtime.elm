module Enclojure.Runtime exposing (..)

import Array
import Dict
import Enclojure.Extra.Maybe
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Types as Types
    exposing
        ( Arity(..)
        , Callable
        , Continuation
        , Env
        , Exception(..)
        , IO(..)
        , Number(..)
        , Thunk(..)
        , Value(..)
        , fakeLoc
        )
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet


type alias Step =
    ( Result ( Exception, Env ) ( IO, Env ), Maybe Thunk )


emptyEnv : Env
emptyEnv =
    { global = Dict.empty
    , local = Dict.empty
    }


setLocalEnv : String -> Value -> Env -> Env
setLocalEnv key value env =
    { env | local = Dict.insert key value env.local }


setGlobalEnv : String -> Value -> Env -> Env
setGlobalEnv key value env =
    { env | global = Dict.insert key value env.global }


fetchEnv : String -> Dict.Dict String Value -> Maybe Value
fetchEnv =
    Dict.get


emptyCallable : Callable
emptyCallable =
    { arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }


extractVariadic : Maybe (Arity a) -> Maybe ({ args : a, rest : List Value } -> Env -> Continuation -> Step)
extractVariadic arity =
    arity
        |> Maybe.andThen
            (\a ->
                case a of
                    Fixed _ ->
                        Nothing

                    Variadic fn ->
                        Just fn
            )


dispatch : Callable -> List Value -> Env -> Continuation -> Step
dispatch callable args env k =
    case args of
        [] ->
            callable.arity0
                |> Maybe.map
                    (\arity0 ->
                        case arity0 of
                            Fixed fn ->
                                fn () env k

                            Variadic fn ->
                                fn { args = (), rest = [] } env k
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 0", env ), Just (Thunk k) )

        [ a0 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity1
                            |> Maybe.map
                                (\arity1 ->
                                    case arity1 of
                                        Fixed fn ->
                                            fn a0 env k

                                        Variadic fn ->
                                            fn { args = a0, rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 1", env ), Just (Thunk k) )

        [ a0, a1 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity2
                            |> Maybe.map
                                (\arity2 ->
                                    case arity2 of
                                        Fixed fn ->
                                            fn ( a0, a1 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 2", env ), Just (Thunk k) )

        [ a0, a1, a2 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity3
                            |> Maybe.map
                                (\arity3 ->
                                    case arity3 of
                                        Fixed fn ->
                                            fn ( a0, a1, a2 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1, a2 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Exception "Invalid arity 3", env ), Just (Thunk k) )

        a0 :: a1 :: a2 :: rest ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity3
                            |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest } env k)
                    )
                |> Maybe.withDefault
                    ( Err ( Exception ("Invalid arity " ++ String.fromInt (List.length args)), env )
                    , Nothing
                    )


toContinuation : Callable -> { self : Value, k : Continuation } -> Thunk
toContinuation callable { k } =
    Thunk
        (\(Located pos arg) env ->
            case arg of
                List args ->
                    dispatch callable (List.map Located.getValue args) env k
                        |> Tuple.mapFirst
                            (\r ->
                                r
                                    |> Result.map (Tuple.mapFirst (Located pos))
                                    |> Result.mapError (Tuple.mapFirst (Located pos))
                            )

                _ ->
                    ( Err ( Located pos (Exception "Foo"), env ), Nothing )
        )


isTruthy : Value -> Bool
isTruthy val =
    case val of
        Nil ->
            False

        Bool False ->
            False

        _ ->
            True


toSeq : Value -> Result Exception (List (Located Value))
toSeq val =
    case val of
        List l ->
            Ok l

        Vector v ->
            Ok <| Array.toList v

        Set s ->
            Ok <| List.map Located.fakeLoc <| ValueSet.toList s

        Map m ->
            Ok <| List.map (\(( _, Located loc _ ) as entry) -> Located loc (MapEntry entry)) (ValueMap.toList m)

        String s ->
            Ok <| List.map (String.fromChar >> String >> Located.fakeLoc) (String.toList s)

        Nil ->
            Ok []

        _ ->
            Err <| Exception (inspect val ++ " is not a sequence")


inspectLocated : Located Value -> String
inspectLocated (Located { start } value) =
    let
        ( line, offset ) =
            start

        suffix =
            ":" ++ String.fromInt line ++ ":" ++ String.fromInt offset
    in
    inspect value ++ suffix


inspect : Value -> String
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
            inspect (Vector (Array.fromList [ Located fakeLoc k, v ]))

        Set set ->
            List.map (\v -> inspect v) (ValueSet.toList set)
                |> String.join ", "
                |> (\r -> "#{" ++ r ++ "}")

        Symbol name ->
            name

        Throwable (Exception str) ->
            "Exception: " ++ str


print : Value -> String
print value =
    case value of
        String string ->
            string

        Nil ->
            ""

        _ ->
            inspect value


toString : Value -> String
toString value =
    case value of
        String s ->
            s

        _ ->
            print value


pure : (a -> Result Exception IO) -> (a -> Env -> Continuation -> Step)
pure fn =
    \v env k ->
        ( fn v
            |> Result.map (\io -> ( io, env ))
            |> Result.mapError (\err -> ( err, env ))
        , Just (Thunk k)
        )


getFn : String -> Callable
getFn key =
    let
        arity1 mapVal =
            arity2 ( mapVal, Nil )

        arity2 ( mapVal, default ) =
            (case mapVal of
                Map m ->
                    ValueMap.get (Keyword key) m |> Maybe.map Located.getValue

                Set s ->
                    if ValueSet.member (Keyword key) s then
                        Just (Keyword key)

                    else
                        Nothing

                _ ->
                    Just Nil
            )
                |> Maybe.withDefault default
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
        , arity2 = Just <| Fixed <| pure (arity2 >> Const >> Ok)
    }


apply : Located Value -> Located Value -> Env -> Continuation -> Types.Step
apply ((Located fnLoc fnExpr) as fn) arg env k =
    case fnExpr of
        Fn _ callable ->
            ( Ok ( Located.map Const arg, env ), Just (callable { self = fnExpr, k = k }) )

        Keyword key ->
            ( Ok ( Located.map Const arg, env ), Just (toContinuation (getFn key) { self = fnExpr, k = k }) )

        _ ->
            ( Err ( Located fnLoc (Exception (inspectLocated fn ++ " is not a valid callable.")), env )
            , Just (Thunk k)
            )


tryString : Value -> Maybe String
tryString value =
    case value of
        String s ->
            Just s

        _ ->
            Nothing


tryKeyword : Value -> Maybe String
tryKeyword value =
    case value of
        Keyword s ->
            Just s

        _ ->
            Nothing


trySymbol : Value -> Maybe String
trySymbol value =
    case value of
        Symbol s ->
            Just s

        _ ->
            Nothing


tryMap : Value -> Maybe Types.ValueMap
tryMap value =
    case value of
        Map s ->
            Just s

        _ ->
            Nothing


trySequenceOf : (Value -> Maybe a) -> Value -> Maybe (List a)
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


tryDictOf : (Value -> Maybe comparable) -> (Value -> Maybe b) -> Value -> Maybe (Dict.Dict comparable b)
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
