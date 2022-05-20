module Enclojure.Runtime exposing
    ( apply
    , const
    , emptyCallable
    , emptyEnv
    , fetchEnv
    , isTruthy
    , prettyTrace
    , setCurrentStackFrameLocation
    , setGlobalEnv
    , setLocalEnv
    , sideEffect
    , throw
    , toFunction
    , toThunk
    )

import Dict
import Enclojure.Extra.Maybe
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Types as Types
    exposing
        ( Arity(..)
        , Callable
        , Continuation
        , Env
        , Exception(..)
        , IO(..)
        , Number(..)
        , Step
        , Thunk(..)
        , Value(..)
        )
import Enclojure.Value as Value
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet exposing (ValueSet)


emptyEnv : Env io
emptyEnv =
    { global = Dict.empty
    , local = Dict.empty
    , stack = [ { name = "user", location = Located.Unknown } ]
    }


setLocalEnv : String -> Value io -> Env io -> Env io
setLocalEnv key value env =
    { env | local = Dict.insert key value env.local }


setGlobalEnv : String -> Value io -> Env io -> Env io
setGlobalEnv key value env =
    { env | global = Dict.insert key value env.global }


fetchEnv : String -> Dict.Dict String (Value io) -> Maybe (Value io)
fetchEnv =
    Dict.get


emptyCallable : Callable io
emptyCallable =
    { arity0 = Nothing
    , arity1 = Nothing
    , arity2 = Nothing
    , arity3 = Nothing
    }


extractVariadic : Maybe (Arity io a) -> Maybe ({ args : a, rest : List (Value io) } -> Env io -> Continuation io -> Step io)
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


dispatch : Callable io -> List (Value io) -> Env io -> Continuation io -> Step io
dispatch callable args env k =
    case args of
        [] ->
            callable.arity0
                |> Maybe.map
                    (\arity0 ->
                        case arity0 of
                            Fixed fn ->
                                fn () env k

                            Variadic fn ->
                                fn { args = (), rest = [] } env k
                    )
                |> Maybe.withDefault ( Err ( Value.exception "Invalid arity 0" |> throw env, env ), Just (Thunk k) )

        [ a0 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity1
                            |> Maybe.map
                                (\arity1 ->
                                    case arity1 of
                                        Fixed fn ->
                                            fn a0 env k

                                        Variadic fn ->
                                            fn { args = a0, rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Value.exception "Invalid arity 1" |> throw env, env ), Just (Thunk k) )

        [ a0, a1 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity2
                            |> Maybe.map
                                (\arity2 ->
                                    case arity2 of
                                        Fixed fn ->
                                            fn ( a0, a1 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Value.exception "Invalid arity 2" |> throw env, env ), Just (Thunk k) )

        [ a0, a1, a2 ] ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = [ a1, a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = [ a2 ] } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        callable.arity3
                            |> Maybe.map
                                (\arity3 ->
                                    case arity3 of
                                        Fixed fn ->
                                            fn ( a0, a1, a2 ) env k

                                        Variadic fn ->
                                            fn { args = ( a0, a1, a2 ), rest = [] } env k
                                )
                    )
                |> Maybe.withDefault ( Err ( Value.exception "Invalid arity 3" |> throw env, env ), Just (Thunk k) )

        a0 :: a1 :: a2 :: rest ->
            extractVariadic callable.arity0
                |> Maybe.map (\fn -> fn { args = (), rest = args } env k)
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity1
                            |> Maybe.map (\fn -> fn { args = a0, rest = a1 :: a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity2
                            |> Maybe.map (\fn -> fn { args = ( a0, a1 ), rest = a2 :: rest } env k)
                    )
                |> Enclojure.Extra.Maybe.orElse
                    (\_ ->
                        extractVariadic callable.arity3
                            |> Maybe.map (\fn -> fn { args = ( a0, a1, a2 ), rest = rest } env k)
                    )
                |> Maybe.withDefault
                    ( Err
                        ( Value.exception ("Invalid arity " ++ String.fromInt (List.length args)) |> throw env
                        , env
                        )
                    , Nothing
                    )


toThunk : Callable io -> { self : Value io, k : Continuation io } -> Thunk io
toThunk callable { k } =
    Thunk
        (\(Located pos arg) env ->
            case arg of
                List args ->
                    dispatch callable (List.map Located.getValue args) env k
                        |> Located pos

                _ ->
                    Located pos ( Err ( Value.exception "Foo" |> throw env, env ), Nothing )
        )


isTruthy : Value io -> Bool
isTruthy val =
    case val of
        Nil ->
            False

        Bool False ->
            False

        _ ->
            True


toFunction : (a -> Result Exception (IO io)) -> (a -> Env io -> Continuation io -> Step io)
toFunction fn =
    \v env k ->
        ( fn v
            |> Result.map (\io -> ( io, env ))
            |> Result.mapError (\err -> ( setStackTrace env.stack err, env ))
        , Just (Thunk k)
        )


getFn : String -> Callable io
getFn key =
    let
        arity1 mapVal =
            arity2 ( mapVal, Nil )

        arity2 ( mapVal, default ) =
            (case mapVal of
                Map m ->
                    ValueMap.get (Keyword key) m |> Maybe.map Located.getValue

                Set s ->
                    if ValueSet.member (Keyword key) s then
                        Just (Keyword key)

                    else
                        Nothing

                Nil ->
                    Nothing

                _ ->
                    Just Nil
            )
                |> Maybe.withDefault default
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| toFunction (arity1 >> Const >> Ok)
        , arity2 = Just <| Fixed <| toFunction (arity2 >> Const >> Ok)
    }


setLookupFn : ValueSet io -> Callable io
setLookupFn set =
    let
        arity1 val =
            if ValueSet.member val set then
                val

            else
                Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| toFunction (arity1 >> Const >> Ok)
    }


mapLookupFn : ValueMap io -> Callable io
mapLookupFn map =
    let
        arity1 val =
            ValueMap.get val map |> Maybe.map Located.getValue |> Maybe.withDefault Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| toFunction (arity1 >> Const >> Ok)
    }


apply : Located (Value io) -> Located (Value io) -> Env io -> Continuation io -> Located (Step io)
apply ((Located fnLoc fnExpr) as fn) arg inputEnv inputK =
    let
        currentStack =
            inputEnv.stack
                |> List.head
                |> Maybe.map (\frame -> { frame | location = fnLoc } :: List.drop 1 inputEnv.stack)
                |> Maybe.withDefault inputEnv.stack

        k =
            \v kEnv -> inputK v { kEnv | stack = List.drop 1 kEnv.stack }
    in
    case fnExpr of
        Fn name callable ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = name |> Maybe.withDefault "fn"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (callable { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Keyword key ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = key
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (toThunk (getFn key) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Map map ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = "Map"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (toThunk (mapLookupFn map) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        Set set ->
            let
                env =
                    { inputEnv
                        | stack =
                            { name = "Set"
                            , location = fnLoc
                            }
                                :: currentStack
                    }
            in
            ( Ok ( Const <| Located.getValue arg, env ), Just (toThunk (setLookupFn set) { self = fnExpr, k = k }) )
                |> Located.sameAs arg

        _ ->
            ( Err
                ( Value.exception (Value.inspectLocated fn ++ " is not a valid callable.") |> throw inputEnv
                , inputEnv
                )
            , Just (Thunk k)
            )
                |> Located fnLoc


setStackTrace : List Types.StackFrame -> Exception -> Exception
setStackTrace stack (Exception msg _) =
    Exception msg stack


setCurrentStackFrameLocation : Located.Location -> Env io -> Env io
setCurrentStackFrameLocation location env =
    let
        newStack =
            env.stack
                |> List.head
                |> Maybe.map (\currentFrame -> { currentFrame | location = location } :: List.drop 1 env.stack)
                |> Maybe.withDefault env.stack
    in
    { env | stack = newStack }


prettyTrace : Exception -> List String
prettyTrace (Exception _ trace) =
    trace
        |> List.map
            (\frame ->
                frame.name
                    ++ (case frame.location of
                            Located.Unknown ->
                                ""

                            Located.Known { start } ->
                                ":" ++ (Tuple.first start |> String.fromInt)
                       )
            )


throw : Env io -> Exception -> Exception
throw env (Exception msg _) =
    Exception msg env.stack


sideEffect : io -> IO io
sideEffect =
    SideEffect


const : Value io -> IO io
const =
    Const
