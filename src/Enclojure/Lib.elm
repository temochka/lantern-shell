module Enclojure.Lib exposing (div, isEqual, isGreaterThan, isGreaterThanOrEqual, isLessThan, isLessThanOrEqual, isNotEqual, list, minus, mul, not_, plus, sleep)

import Enclojure.Located exposing (Located(..))
import Enclojure.Runtime as Runtime exposing (Arity(..), Callable, Env, Exception(..), IO(..), Thunk(..), Value(..), emptyCallable, inspect)


fakeLoc : { start : ( number, number ), end : ( number, number ) }
fakeLoc =
    { start = ( 0, 0 ), end = ( 0, 0 ) }


pure : (a -> Result Exception IO) -> (a -> Env -> Thunk -> ( Result Exception ( IO, Env ), Maybe Thunk ))
pure fn =
    \v env k -> ( fn v |> Result.map (\io -> ( io, env )), Just k )


sleep : Callable
sleep =
    let
        arity1 val =
            case val of
                Int i ->
                    Ok (Sleep (toFloat i))

                _ ->
                    Err (Exception "type error: sleep expects one integer argument")
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure arity1))
    }


not_ : Callable
not_ =
    let
        arity1 val =
            Ok (Const (Bool (not (Runtime.isTruthy val))))
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure arity1))
    }


list : Callable
list =
    let
        arity0 { rest } =
            Ok (Const (List (List.map (Located fakeLoc) rest)))
    in
    { emptyCallable | arity0 = Just (Variadic (pure arity0)) }


plus : Callable
plus =
    numOp { identity = 0, intOp = (+), floatOp = (+) }


minus : Callable
minus =
    numOp { identity = 0, intOp = (-), floatOp = (-) }


mul : Callable
mul =
    numOp { identity = 1, intOp = (*), floatOp = (*) }


div : Callable
div =
    numOp { identity = 1, intOp = (//), floatOp = (/) }


numOp : { identity : Int, intOp : Int -> Int -> Int, floatOp : Float -> Float -> Float } -> Callable
numOp { identity, intOp, floatOp } =
    let
        arity0 _ =
            Ok (Int identity)

        arity1 val =
            arity2 { args = ( Int identity, val ), rest = [] }

        arity2 { args, rest } =
            let
                result =
                    case args of
                        ( Float a, Float b ) ->
                            Ok (Float (floatOp a b))

                        ( Int a, Int b ) ->
                            Ok (Int (intOp a b))

                        ( Float a, Int b ) ->
                            Ok (Float (floatOp a (toFloat b)))

                        ( Int a, Float b ) ->
                            Ok (Float (floatOp (toFloat a) b))

                        ( Int _, b ) ->
                            Err (Exception (inspect b ++ " is not a number"))

                        ( Float _, b ) ->
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


isLessThan : Callable
isLessThan =
    compOp { intOp = (<), floatOp = (<) }


isLessThanOrEqual : Callable
isLessThanOrEqual =
    compOp { intOp = (<=), floatOp = (<=) }


isGreaterThan : Callable
isGreaterThan =
    compOp { intOp = (>), floatOp = (>) }


isGreaterThanOrEqual : Callable
isGreaterThanOrEqual =
    compOp { intOp = (>=), floatOp = (>=) }


compOp : { intOp : Int -> Int -> Bool, floatOp : Float -> Float -> Bool } -> Callable
compOp { intOp, floatOp } =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                result =
                    case args of
                        ( Float a, Float b ) ->
                            Ok (floatOp a b)

                        ( Float a, Int b ) ->
                            Ok (floatOp a (toFloat b))

                        ( Int a, Float b ) ->
                            Ok (floatOp (toFloat a) b)

                        ( Int a, Int b ) ->
                            Ok (intOp a b)

                        ( a, b ) ->
                            Err (Exception ("can't compare " ++ inspect a ++ " and " ++ inspect b))
            in
            case rest of
                [] ->
                    result |> Result.map Bool

                nextVal1 :: nextRest ->
                    result
                        |> Result.andThen
                            (\r ->
                                if r then
                                    arity2 { args = ( Tuple.second args, nextVal1 ), rest = nextRest }

                                else
                                    Ok (Bool False)
                            )
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }


isEqual : Callable
isEqual =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (a == b))

                nextVal1 :: nextRest ->
                    if a == b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }


isNotEqual : Callable
isNotEqual =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (a /= b))

                nextVal1 :: nextRest ->
                    if a /= b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }
