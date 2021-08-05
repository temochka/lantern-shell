module Enclojure.Lib.String exposing (splitLines)

import Enclojure.Located as Located
import Enclojure.Runtime exposing (emptyCallable, inspect, pure)
import Enclojure.Types as Types exposing (Arity(..), Exception(..), IO(..), Value(..))


splitLines : Types.Callable
splitLines =
    let
        arity1 val =
            case val of
                String s ->
                    String.lines s
                        |> List.map (String >> Located.fakeLoc)
                        |> Types.List
                        |> Ok

                _ ->
                    Err (Exception ("type error: expected string, got " ++ inspect val))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Result.map Const)
    }
