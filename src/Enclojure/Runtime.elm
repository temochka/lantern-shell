module Enclojure.Runtime exposing (..)

import Dict
import Enclojure.Extra.Maybe
import Enclojure.Located as Located exposing (Located(..))


type Exception
    = Exception String


type Thunk
    = Thunk (Located Value -> ( Result (Located Exception) (Located IO), Maybe Thunk ))


type Arity a
    = Fixed (a -> Thunk -> ( Result Exception IO, Maybe Thunk ))
    | Variadic ({ args : a, rest : List Value } -> Thunk -> ( Result Exception IO, Maybe Thunk ))


type IO
    = Const Value
    | Sleep Float


type alias Callable =
    { arity0 : Maybe (Arity ())
    , arity1 : Maybe (Arity Value)
    , arity2 : Maybe (Arity ( Value, Value ))
    , arity3 : Maybe (Arity ( Value, Value, Value ))
    }


type alias Number =
    Int


type Value
    = Number Number
    | Ref String (Located Value)
    | Fn (Maybe String) (Thunk -> Thunk)
    | List (List (Located Value))
    | Vector (List (Located Value))
    | Nil
    | Bool Basics.Bool
    | Symbol String


type alias Env =
    Dict.Dict String Value


emptyCallable : Callable
emptyCallable =
    { arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }


extractVariadic : Maybe (Arity a) -> Maybe ({ args : a, rest : List Value } -> Thunk -> ( Result Exception IO, Maybe Thunk ))
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


dispatch : Callable -> List Value -> Thunk -> ( Result Exception IO, Maybe Thunk )
dispatch callable args k =
    case args of
        [] ->
            callable.arity0
                |> Maybe.map
                    (\arity0 ->
                        case arity0 of
                            Fixed fn ->
                                fn () k

                            Variadic fn ->
                                fn { args = (), rest = [] } k
                    )
                |> Maybe.withDefault ( Err (Exception "Invalid arity 0"), Just k )

        [ a0 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity1
                            |> Maybe.map
                                (\arity1 ->
                                    case arity1 of
                                        Fixed fn ->
                                            fn a0 k

                                        Variadic fn ->
                                            fn { args = a0, rest = [] } k
                                )
                    )
                |> Maybe.withDefault ( Err (Exception "Invalid arity 1"), Just k )

        [ a0, a1 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] } k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity2
                            |> Maybe.map
                                (\arity2 ->
                                    case arity2 of
                                        Fixed fn ->
                                            fn ( a0, a1 ) k

                                        Variadic fn ->
                                            fn { args = ( a0, a1 ), rest = [] } k
                                )
                    )
                |> Maybe.withDefault ( Err (Exception "Invalid arity 2"), Just k )

        [ a0, a1, a2 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] } k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] } k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity3
                            |> Maybe.map
                                (\arity3 ->
                                    case arity3 of
                                        Fixed fn ->
                                            fn ( a0, a1, a2 ) k

                                        Variadic fn ->
                                            fn { args = ( a0, a1, a2 ), rest = [] } k
                                )
                    )
                |> Maybe.withDefault ( Err (Exception "Invalid arity 3"), Just k )

        a0 :: a1 :: a2 :: rest ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest } k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest } k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity3
                            |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest } k)
                    )
                |> Maybe.withDefault ( Err (Exception ("Invalid arity " ++ String.fromInt (List.length args))), Nothing )


toContinuation : Callable -> Thunk -> Thunk
toContinuation callable k =
    Thunk
        (\(Located pos arg) ->
            case arg of
                List args ->
                    dispatch callable (List.map Located.getValue args) k
                        |> Tuple.mapFirst
                            (\r ->
                                r
                                    |> Result.map (Located pos)
                                    |> Result.mapError (Located pos)
                            )

                _ ->
                    ( Err (Located pos (Exception "Foo")), Nothing )
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

        Number x ->
            String.fromInt x

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
            "[" ++ (List.map (Located.getValue >> inspect) l |> String.join " ") ++ "]"

        Symbol name ->
            name
