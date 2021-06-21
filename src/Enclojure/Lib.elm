module Enclojure.Lib exposing (div, minus, mul, plus)

import Enclojure.Runtime exposing (Arity(..), Callable, Exception(..), Value(..), emptyCallable, inspect)


plus : Callable
plus =
    intOp { identity = 0, op = (+) }


minus : Callable
minus =
    intOp { identity = 0, op = (-) }


mul : Callable
mul =
    intOp { identity = 1, op = (*) }


div : Callable
div =
    intOp { identity = 1, op = (//) }


intOp : { identity : Int, op : Int -> Int -> Int } -> Callable
intOp { identity, op } =
    let
        arity0 _ =
            Ok (Number identity)

        arity1 val =
            arity2 { args = ( Number identity, val ), rest = [] }

        arity2 { args, rest } =
            let
                result =
                    case args of
                        ( Number a, Number b ) ->
                            Ok (Number (op a b))

                        ( Number _, invalid ) ->
                            Err (Exception (inspect invalid ++ " is not a number"))

                        ( invalid, _ ) ->
                            Err (Exception (inspect invalid ++ " is not a number"))
            in
            case rest of
                [] ->
                    result

                nextVal1 :: nextRest ->
                    result |> Result.andThen (\nextVal0 -> arity2 { args = ( nextVal0, nextVal1 ), rest = nextRest })
    in
    { emptyCallable
        | arity0 = Just (Fixed arity0)
        , arity1 = Just (Fixed arity1)
        , arity2 = Just (Variadic arity2)
    }
