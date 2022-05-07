module Enclojure.Lib.String exposing (init)

import Enclojure.Located as Located
import Enclojure.Runtime as Runtime exposing (emptyCallable, inspect, pure)
import Enclojure.Types as Types exposing (Arity(..), Exception(..), IO(..), Value(..))


init : Types.Env -> Types.Env
init env =
    env
        |> Runtime.setGlobalEnv "string/join"
            (Fn (Just "string/join") (Runtime.toContinuation join))
        |> Runtime.setGlobalEnv "string/length"
            (Fn (Just "string/length") (Runtime.toContinuation length))
        |> Runtime.setGlobalEnv "string/split-lines"
            (Fn (Just "string/split-lines") (Runtime.toContinuation splitLines))


splitLines : Types.Callable
splitLines =
    let
        arity1 val =
            case val of
                String s ->
                    String.lines s
                        |> List.map (String >> Located.unknown)
                        |> Types.List
                        |> Ok

                _ ->
                    Err (Exception ("type error: expected string, got " ++ inspect val) [])
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Result.map Const)
    }


length : Types.Callable
length =
    let
        arity1 val =
            Runtime.tryString val
                |> Maybe.map (String.length >> Types.Int >> Types.Number >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected string, got " ++ inspect val) []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Result.map Const)
    }


join : Types.Callable
join =
    let
        arity1 val =
            val
                |> Runtime.trySequenceOf (Runtime.toString >> Just)
                |> Maybe.map (String.join "" >> Types.String >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected a sequence, got " ++ inspect val) []))

        arity2 ( sepVal, collVal ) =
            Maybe.map2 (\sep coll -> String.join sep coll |> Types.String |> Ok)
                (Runtime.tryString sepVal)
                (Runtime.trySequenceOf (Runtime.toString >> Just) collVal)
                |> Maybe.withDefault (Err (Exception "type error: expected a separator and a sequence of strings" []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Result.map Const)
        , arity2 = Just <| Fixed <| pure (arity2 >> Result.map Const)
    }
