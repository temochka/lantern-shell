module Enclojure.Lib exposing (div, minus, mul, plus, sleep)

import Enclojure.Runtime exposing (Arity(..), Callable, Env, Exception(..), IO(..), Thunk(..), Value(..), emptyCallable, inspect)


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


pure : (a -> Result Exception IO) -> (a -> Env -> Thunk -> ( Result Exception ( IO, Env ), Maybe Thunk ))
pure fn =
    \v env k -> ( fn v |> Result.map (\io -> ( io, env )), Just k )


sleep : Callable
sleep =
    let
        arity1 val =
            case val of
                Number i ->
                    Ok (Sleep (toFloat i))

                _ ->
                    Err (Exception "type error: sleep expects one integer argument")
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure arity1))
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
        | arity0 = Just (Fixed (pure (arity0 >> Result.map Const)))
        , arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }
