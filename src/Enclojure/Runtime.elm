module Enclojure.Runtime exposing (..)

import Dict
import Enclojure.Extra.Maybe
import Enclojure.Located exposing (Located(..))
import Enclojure.Parser as Parser


type Exception
    = Exception String



-- type Arity a
--     = Fixed (a -> Result Exception Value)
--     | Variadic ({ args : a, rest : List Value } -> Result Exception Value)


type Arity a
    = Fixed (a -> Value)
    | Variadic ({ args : a, rest : List Value } -> Value)


type alias Callable =
    { arity0 : Maybe (Arity ())
    , arity1 : Maybe (Arity Value)
    , arity2 : Maybe (Arity ( Value, Value ))
    , arity3 : Maybe (Arity ( Value, Value, Value ))
    }


type Value
    = Number Parser.Number
    | Fn (Maybe String) Callable
    | List (List (Located Value))
    | Nil


type alias Env =
    Dict.Dict String Value


emptyCallable : Callable
emptyCallable =
    { arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }



-- invoke : Callable -> List Value -> Result Exception Value
-- invoke callable args =
--     let
--         extractVariadic arity =
--             arity
--                 |> Maybe.andThen
--                     (\a ->
--                         case a of
--                             Fixed _ ->
--                                 Nothing
--                             Variadic fn ->
--                                 Just fn
--                     )
--     in
--     case args of
--         [] ->
--             callable.arity0
--                 |> Result.fromMaybe (Exception "Invalid arity 0")
--                 |> Result.andThen
--                     (\arity0 ->
--                         case arity0 of
--                             Fixed fn ->
--                                 fn ()
--                             Variadic fn ->
--                                 fn { args = (), rest = [] }
--                     )
--         [ a0 ] ->
--             extractVariadic callable.arity0
--                 |> Maybe.map (\fn -> fn { args = (), rest = args })
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         callable.arity1
--                             |> Maybe.map
--                                 (\arity1 ->
--                                     case arity1 of
--                                         Fixed fn ->
--                                             fn a0
--                                         Variadic fn ->
--                                             fn { args = a0, rest = [] }
--                                 )
--                     )
--                 |> Maybe.withDefault (Err (Exception "Invalid arity 1"))
--         [ a0, a1 ] ->
--             extractVariadic callable.arity0
--                 |> Maybe.map (\fn -> fn { args = (), rest = args })
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity1
--                             |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] })
--                     )
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         callable.arity2
--                             |> Maybe.map
--                                 (\arity2 ->
--                                     case arity2 of
--                                         Fixed fn ->
--                                             fn ( a0, a1 )
--                                         Variadic fn ->
--                                             fn { args = ( a0, a1 ), rest = [] }
--                                 )
--                     )
--                 |> Maybe.withDefault (Err (Exception "Invalid arity 2"))
--         [ a0, a1, a2 ] ->
--             extractVariadic callable.arity0
--                 |> Maybe.map (\fn -> fn { args = (), rest = args })
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity1
--                             |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] })
--                     )
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity2
--                             |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] })
--                     )
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         callable.arity3
--                             |> Maybe.map
--                                 (\arity3 ->
--                                     case arity3 of
--                                         Fixed fn ->
--                                             fn ( a0, a1, a2 )
--                                         Variadic fn ->
--                                             fn { args = ( a0, a1, a2 ), rest = [] }
--                                 )
--                     )
--                 |> Maybe.withDefault (Err (Exception "Invalid arity 3"))
--         a0 :: a1 :: a2 :: rest ->
--             extractVariadic callable.arity0
--                 |> Maybe.map (\fn -> fn { args = (), rest = args })
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity1
--                             |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest })
--                     )
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity2
--                             |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest })
--                     )
--                 |> Enclojure.Extra.Maybe.orElse
--                     (\_ ->
--                         extractVariadic callable.arity3
--                             |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest })
--                     )
--                 |> Maybe.withDefault (Err (Exception ("Invalid arity " ++ String.fromInt (List.length args))))


invoke : Callable -> List Value -> Value
invoke callable args =
    let
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
    in
    case args of
        [] ->
            callable.arity0
                |> Maybe.map
                    (\arity0 ->
                        case arity0 of
                            Fixed fn ->
                                fn ()

                            Variadic fn ->
                                fn { args = (), rest = [] }
                    )
                |> Maybe.withDefault Nil

        [ a0 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args })
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity1
                            |> Maybe.map
                                (\arity1 ->
                                    case arity1 of
                                        Fixed fn ->
                                            fn a0

                                        Variadic fn ->
                                            fn { args = a0, rest = [] }
                                )
                    )
                |> Maybe.withDefault Nil

        [ a0, a1 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args })
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] })
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity2
                            |> Maybe.map
                                (\arity2 ->
                                    case arity2 of
                                        Fixed fn ->
                                            fn ( a0, a1 )

                                        Variadic fn ->
                                            fn { args = ( a0, a1 ), rest = [] }
                                )
                    )
                |> Maybe.withDefault Nil

        [ a0, a1, a2 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args })
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] })
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] })
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity3
                            |> Maybe.map
                                (\arity3 ->
                                    case arity3 of
                                        Fixed fn ->
                                            fn ( a0, a1, a2 )

                                        Variadic fn ->
                                            fn { args = ( a0, a1, a2 ), rest = [] }
                                )
                    )
                |> Maybe.withDefault Nil

        a0 :: a1 :: a2 :: rest ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args })
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest })
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest })
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity3
                            |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest })
                    )
                |> Maybe.withDefault Nil


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
        Number x ->
            "int<" ++ String.fromInt x ++ ">"

        Fn name _ ->
            "fn<" ++ (name |> Maybe.withDefault "anonymous") ++ ">"

        List _ ->
            "list"

        Nil ->
            "nil"
