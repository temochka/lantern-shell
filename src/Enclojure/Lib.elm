module Enclojure.Lib exposing (div, minus, mul, plus, sleep)

import Enclojure.Runtime exposing (Arity(..), Callable, Exception(..), IO(..), Value(..), emptyCallable, inspect)


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


sleep : Callable
sleep =
    let
        arity1 val =
            case val of
                Number i ->
                    Ok (Sleep (toFloat i))

                _ ->
                    Ok (Const Nil)
    in
    { emptyCallable
        | arity1 = Just (Fixed arity1)
    }


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

                        ( Number _, b ) ->
                            Err (Exception (inspect b ++ " is not a number"))

                        ( a, _ ) ->
                            Err (Exception (inspect a ++ " is not a number"))
            in
            case rest of
                [] ->
                    result

                nextVal1 :: nextRest ->
                    result
                        |> Result.andThen
                            (\x ->
                                arity2 { args = ( x, nextVal1 ), rest = nextRest }
                            )
    in
    { emptyCallable
        | arity0 = Just (Fixed (arity0 >> Result.map Const))
        , arity1 = Just (Fixed (arity1 >> Result.map Const))
        , arity2 = Just (Variadic (arity2 >> Result.map Const))
    }
