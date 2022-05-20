module Enclojure.Lib.String exposing (init)

import Enclojure.Callable as Callable
import Enclojure.Located as Located
import Enclojure.Runtime as Runtime
import Enclojure.Types as Types exposing (Arity(..), Callable, Exception(..), IO(..), Value(..))
import Enclojure.Value as Value exposing (inspect)


emptyCallable : Callable io
emptyCallable =
    Callable.new


init : Types.Env io -> Types.Env io
init env =
    env
        |> Runtime.setGlobalEnv "string/join"
            (Fn (Just "string/join") (Callable.toThunk join))
        |> Runtime.setGlobalEnv "string/length"
            (Fn (Just "string/length") (Callable.toThunk length))
        |> Runtime.setGlobalEnv "string/split-lines"
            (Fn (Just "string/split-lines") (Callable.toThunk splitLines))


splitLines : Types.Callable io
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
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
    }


length : Types.Callable io
length =
    let
        arity1 val =
            Value.tryString val
                |> Maybe.map (String.length >> Types.Int >> Types.Number >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected string, got " ++ inspect val) []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
    }


join : Types.Callable io
join =
    let
        arity1 val =
            val
                |> Value.trySequenceOf (Value.toString >> Just)
                |> Maybe.map (String.join "" >> Types.String >> Ok)
                |> Maybe.withDefault (Err (Exception ("type error: expected a sequence, got " ++ inspect val) []))

        arity2 ( sepVal, collVal ) =
            Maybe.map2 (\sep coll -> String.join sep coll |> Types.String |> Ok)
                (Value.tryString sepVal)
                (Value.trySequenceOf (Value.toString >> Just) collVal)
                |> Maybe.withDefault (Err (Exception "type error: expected a separator and a sequence of strings" []))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| Callable.toArityFunction (arity1 >> Result.map Const)
        , arity2 = Just <| Fixed <| Callable.toArityFunction (arity2 >> Result.map Const)
    }
