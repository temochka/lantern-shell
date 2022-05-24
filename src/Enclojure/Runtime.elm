module Enclojure.Runtime exposing
    ( apply
    , bindGlobal
    , bindLocal
    , const
    , emptyEnv
    , fetchEnv
    , isTruthy
    , prettyTrace
    , setCurrentStackFrameLocation
    , sideEffect
    , throw
    , toFunction
    )

import Dict
import Enclojure.Callable as Callable exposing (toThunk)
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
        , Scope
        , Step
        , Thunk(..)
        , Value(..)
        )
import Enclojure.Value as Value
import Enclojure.ValueMap as ValueMap exposing (ValueMap)
import Enclojure.ValueSet as ValueSet exposing (ValueSet)


emptyCallable : Callable io
emptyCallable =
    Callable.new


newScope : Scope io
newScope =
    { atoms = Dict.empty
    , atomIdGenerator = 0
    , bindings = Dict.empty
    }


emptyEnv : Env io
emptyEnv =
    { globalScope = newScope
    , localScope = newScope
    , stack = [ { name = "user", location = Located.Unknown } ]
    }


bindLocal : String -> Value io -> Env io -> Env io
bindLocal key value ({ localScope } as env) =
    let
        newLocalScope =
            { localScope | bindings = Dict.insert key value localScope.bindings }
    in
    { env | localScope = newLocalScope }


bindGlobal : String -> Value io -> Env io -> Env io
bindGlobal key value ({ globalScope } as env) =
    let
        newGlobalScope =
            { globalScope | bindings = Dict.insert key value globalScope.bindings }
    in
    { env | globalScope = newGlobalScope }


fetchEnv : String -> Scope io -> Maybe (Value io)
fetchEnv name scope =
    Dict.get name scope.bindings


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
