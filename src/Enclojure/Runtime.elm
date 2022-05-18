module Enclojure.Runtime exposing (..)

import Array
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
        , Thunk(..)
        , Value(..)
        )
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet


type alias Step io =
    ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) )


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
                |> Maybe.withDefault ( Err ( exception env "Invalid arity 0", env ), Just (Thunk k) )

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
                |> Maybe.withDefault ( Err ( exception env "Invalid arity 1", env ), Just (Thunk k) )

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
                |> Maybe.withDefault ( Err ( exception env "Invalid arity 2", env ), Just (Thunk k) )

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
                |> Maybe.withDefault ( Err ( exception env "Invalid arity 3", env ), Just (Thunk k) )

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
                    ( Err ( exception env ("Invalid arity " ++ String.fromInt (List.length args)), env )
                    , Nothing
                    )


toThunk : Callable io -> { self : Value io, k : Continuation io } -> Thunk io
toThunk callable { k } =
    Thunk
        (\(Located pos arg) env ->
            case arg of
                List args ->
                    dispatch callable (List.map Located.getValue args) env k
                        |> Tuple.mapFirst
                            (\r ->
                                r
                                    |> Result.map (Tuple.mapFirst (Located pos))
                                    |> Result.mapError (Tuple.mapFirst (Located pos))
                            )

                _ ->
                    ( Err ( Located pos (exception env "Foo"), env ), Nothing )
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


toSeq : Value io -> Result Exception (List (Located (Value io)))
toSeq val =
    case val of
        List l ->
            Ok l

        Vector v ->
            Ok <| Array.toList v

        Set s ->
            Ok <| List.map Located.unknown <| ValueSet.toList s

        Map m ->
            Ok <| List.map (\(( _, Located loc _ ) as entry) -> Located loc (MapEntry entry)) (ValueMap.toList m)

        String s ->
            Ok <| List.map (String.fromChar >> String >> Located.unknown) (String.toList s)

        MapEntry ( k, v ) ->
            Ok <| [ Located.sameAs v k, v ]

        Nil ->
            Ok []

        _ ->
            Err <| Exception (inspect val ++ " is not a sequence") []


toMap : Value io -> Maybe (Types.ValueMap io)
toMap val =
    case val of
        Nil ->
            Just ValueMap.empty

        Vector vec ->
            Array.toList vec |> List.indexedMap (\i e -> ( Number <| Int i, e )) |> ValueMap.fromList |> Just

        Map m ->
            Just m

        _ ->
            Nothing


inspectLocated : Located (Value io) -> String
inspectLocated locatedValue =
    let
        suffix =
            locatedValue
                |> Located.getOffsets
                |> Maybe.map (\{ start } -> ":" ++ String.fromInt (Tuple.first start) ++ ":" ++ String.fromInt (Tuple.second start))
                |> Maybe.withDefault ""
    in
    inspect (Located.getValue locatedValue) ++ suffix


inspect : Value io -> String
inspect value =
    case value of
        Ref name _ ->
            "#'" ++ name

        String string ->
            "\"" ++ string ++ "\""

        Number (Int x) ->
            String.fromInt x

        Number (Float x) ->
            String.fromFloat x

        Fn name _ ->
            "fn<" ++ (name |> Maybe.withDefault "anonymous") ++ ">"

        List l ->
            "(" ++ (List.map (Located.getValue >> inspect) l |> String.join " ") ++ ")"

        Nil ->
            "nil"

        Bool b ->
            if b then
                "true"

            else
                "false"

        Vector l ->
            "[" ++ (List.map (Located.getValue >> inspect) (Array.toList l) |> String.join " ") ++ "]"

        Keyword name ->
            ":" ++ name

        Map m ->
            List.map (\( k, Located _ v ) -> inspect k ++ " " ++ inspect v) (ValueMap.toList m)
                |> String.join ", "
                |> (\r -> "{" ++ r ++ "}")

        MapEntry ( k, v ) ->
            inspect (Vector (Array.fromList [ Located.unknown k, v ]))

        Set set ->
            List.map (\v -> inspect v) (ValueSet.toList set)
                |> String.join ", "
                |> (\r -> "#{" ++ r ++ "}")

        Symbol name ->
            name

        Throwable (Exception str _) ->
            "Exception: " ++ str


print : Value io -> String
print value =
    case value of
        String string ->
            string

        Nil ->
            ""

        _ ->
            inspect value


toString : Value io -> String
toString value =
    case value of
        String s ->
            s

        _ ->
            print value


pure : (a -> Result Exception (IO io)) -> (a -> Env io -> Continuation io -> Step io)
pure fn =
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
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
        , arity2 = Just <| Fixed <| pure (arity2 >> Const >> Ok)
    }


setLookupFn : Types.ValueSet io -> Callable io
setLookupFn set =
    let
        arity1 val =
            if ValueSet.member val set then
                val

            else
                Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
    }


mapLookupFn : Types.ValueMap io -> Callable io
mapLookupFn map =
    let
        arity1 val =
            ValueMap.get val map |> Maybe.map Located.getValue |> Maybe.withDefault Nil
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
    }


apply : Located (Value io) -> Located (Value io) -> Env io -> Continuation io -> Types.Step io
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
            ( Ok ( Located.map Const arg, env ), Just (callable { self = fnExpr, k = k }) )

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
            ( Ok ( Located.map Const arg, env ), Just (toThunk (getFn key) { self = fnExpr, k = k }) )

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
            ( Ok ( Located.map Const arg, env ), Just (toThunk (mapLookupFn map) { self = fnExpr, k = k }) )

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
            ( Ok ( Located.map Const arg, env ), Just (toThunk (setLookupFn set) { self = fnExpr, k = k }) )

        _ ->
            ( Err ( Located fnLoc (exception inputEnv (inspectLocated fn ++ " is not a valid callable.")), inputEnv )
            , Just (Thunk k)
            )


tryString : Value io -> Maybe String
tryString value =
    case value of
        String s ->
            Just s

        _ ->
            Nothing


tryKeyword : Value io -> Maybe String
tryKeyword value =
    case value of
        Keyword s ->
            Just s

        _ ->
            Nothing


trySymbol : Value io -> Maybe String
trySymbol value =
    case value of
        Symbol s ->
            Just s

        _ ->
            Nothing


tryMap : Value io -> Maybe (Types.ValueMap io)
tryMap value =
    case value of
        Map s ->
            Just s

        _ ->
            Nothing


trySequenceOf : (Value io -> Maybe a) -> Value io -> Maybe (List a)
trySequenceOf extract value =
    let
        extractAll sequence =
            sequence
                |> List.foldr
                    (\e a ->
                        a
                            |> Maybe.andThen
                                (\acc ->
                                    extract e
                                        |> Maybe.map (\extracted -> extracted :: acc)
                                )
                    )
                    (Just [])
    in
    toSeq value
        |> Result.map (List.map Located.getValue)
        |> Result.toMaybe
        |> Maybe.andThen extractAll


tryDictOf : (Value io -> Maybe comparable) -> (Value io -> Maybe b) -> Value io -> Maybe (Dict.Dict comparable b)
tryDictOf extractKey extractValue value =
    let
        extractAll kvSequence =
            kvSequence
                |> List.foldr
                    (\( key, val ) a ->
                        a
                            |> Maybe.andThen
                                (\acc ->
                                    Maybe.map2
                                        (\extractedKey extractedVal -> ( extractedKey, extractedVal ) :: acc)
                                        (extractKey key)
                                        (extractValue (Located.getValue val))
                                )
                    )
                    (Just [])
    in
    case value of
        Map m ->
            m |> ValueMap.toList |> extractAll |> Maybe.map Dict.fromList

        _ ->
            Nothing


exception : Env io -> String -> Exception
exception env message =
    Exception message env.stack


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
prettyTrace (Exception msg trace) =
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
