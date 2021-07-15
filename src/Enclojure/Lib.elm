module Enclojure.Lib exposing (apply, div, filter, isEqual, isGreaterThan, isGreaterThanOrEqual, isLessThan, isLessThanOrEqual, isNotEqual, list, map, minus, mul, not_, plus, seq, sleep, str)

import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime exposing (emptyCallable, inspect)
import Enclojure.Types exposing (..)
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet


pure : (a -> Result Exception IO) -> (a -> Env -> Thunk -> ( Result Exception ( IO, Env ), Maybe Thunk ))
pure fn =
    \v env k -> ( fn v |> Result.map (\io -> ( io, env )), Just k )


apply : Located Value -> Located Value -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
apply ((Located fnLoc fnExpr) as fn) arg env k =
    case fnExpr of
        Fn _ callable ->
            ( Ok ( Located.map Const arg, env ), Just (callable { self = fnExpr, k = k }) )

        _ ->
            ( Err (Located fnLoc (Exception (Runtime.inspectLocated fn ++ " is not a valid callable."))), Just k )


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
    compOp { intOp = (<), floatOp = (<), stringOp = (<) }


isLessThanOrEqual : Callable
isLessThanOrEqual =
    compOp { intOp = (<=), floatOp = (<=), stringOp = (<=) }


isGreaterThan : Callable
isGreaterThan =
    compOp { intOp = (>), floatOp = (>), stringOp = (<=) }


isGreaterThanOrEqual : Callable
isGreaterThanOrEqual =
    compOp { intOp = (>=), floatOp = (>=), stringOp = (>=) }


compOp : { intOp : Int -> Int -> Bool, floatOp : Float -> Float -> Bool, stringOp : String -> String -> Bool } -> Callable
compOp { intOp, floatOp, stringOp } =
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

                        ( String a, String b ) ->
                            Ok (stringOp a b)

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


str : Callable
str =
    let
        arity0 { rest } =
            rest
                |> List.map Runtime.toString
                |> String.join ""
                |> String
                |> Ok
    in
    { emptyCallable
        | arity0 = Just (Variadic (pure (arity0 >> Result.map Const)))
    }


seq : Callable
seq =
    let
        arity1 coll =
            case coll of
                List l ->
                    Ok (List l)

                Vector l ->
                    Ok (List l)

                Set s ->
                    Ok (List (ValueSet.toList s |> List.map Located.fakeLoc))

                Map m ->
                    Ok (List (ValueMap.toList m |> List.map (MapEntry >> Located.fakeLoc)))

                _ ->
                    Err (Exception (inspect coll ++ " is not sequable"))
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
    }


fixedCall : Maybe (Arity a) -> a -> Env -> Thunk -> ( Result Exception ( IO, Env ), Maybe Thunk )
fixedCall mArity =
    mArity
        |> Maybe.andThen
            (\arity ->
                case arity of
                    Fixed a ->
                        Just a

                    Variadic _ ->
                        Nothing
            )
        |> Maybe.withDefault (\_ _ k -> ( Err (Exception "Interpreter error: undefined internal call arity"), Just k ))


map : Callable
map =
    let
        arity2 ( f, coll ) env1 (Thunk k) =
            fixedCall
                seq.arity1
                coll
                env1
                (Thunk
                    (\(Located collLoc collSeq) env2 ->
                        case collSeq of
                            List originalColl ->
                                ( Ok ( Located collLoc (Const (List [])), env2 )
                                , originalColl
                                    |> List.foldl
                                        (\e a ->
                                            \(Located locI vI) envI ->
                                                case vI of
                                                    List mappedColl ->
                                                        apply (Located locI f)
                                                            (Located.replace e (List [ e ]))
                                                            envI
                                                            (Thunk
                                                                (\fRet fRetEnv ->
                                                                    a (Located locI (List (fRet :: mappedColl))) fRetEnv
                                                                )
                                                            )

                                                    _ ->
                                                        ( Err (Located locI (Exception "Interpreter error: list evaluation yielded a non-list.")), Just (Thunk a) )
                                        )
                                        k
                                    |> Thunk
                                    |> Just
                                )

                            _ ->
                                ( Err (Located.fakeLoc (Exception "Interpreter error: seq returned a non-list.")), Just (Thunk k) )
                    )
                )
    in
    { emptyCallable
        | arity2 = Just (Fixed arity2)
    }


filter : Callable
filter =
    let
        arity2 ( f, coll ) env1 (Thunk k) =
            fixedCall
                seq.arity1
                coll
                env1
                (Thunk
                    (\(Located collLoc collSeq) env2 ->
                        case collSeq of
                            List originalColl ->
                                ( Ok ( Located collLoc (Const (List [])), env2 )
                                , originalColl
                                    |> List.foldl
                                        (\e a ->
                                            \(Located locI vI) envI ->
                                                case vI of
                                                    List mappedColl ->
                                                        apply (Located locI f)
                                                            (Located.replace e (List [ e ]))
                                                            envI
                                                            (Thunk
                                                                (\fRet fRetEnv ->
                                                                    if Runtime.isTruthy (Located.getValue fRet) then
                                                                        a (Located locI (List (e :: mappedColl))) fRetEnv

                                                                    else
                                                                        a (Located locI (List mappedColl)) fRetEnv
                                                                )
                                                            )

                                                    _ ->
                                                        ( Err (Located locI (Exception "Interpreter error: list evaluation yielded a non-list.")), Just (Thunk a) )
                                        )
                                        k
                                    |> Thunk
                                    |> Just
                                )

                            _ ->
                                ( Err (Located.fakeLoc (Exception "Interpreter error: seq returned a non-list.")), Just (Thunk k) )
                    )
                )
    in
    { emptyCallable
        | arity2 = Just (Fixed arity2)
    }
