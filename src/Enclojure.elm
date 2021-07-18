module Enclojure exposing (eval)

import Enclojure.Extra.Maybe exposing (orElse)
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Parser
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Arity(..), Continuation, Env, Exception(..), IO(..), Number(..), Step, Thunk(..), Value(..))
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Html exposing (a)
import List exposing (map)
import Parser exposing (symbol)


resolveSymbol : Env -> String -> Result Exception Value
resolveSymbol env symbol =
    Runtime.fetchEnv symbol env.local
        |> orElse (\_ -> Runtime.fetchEnv symbol env.global)
        |> Maybe.map Ok
        |> Maybe.withDefault
            (case symbol of
                "+" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.plus))

                "-" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.minus))

                "/" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.div))

                "*" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.mul))

                "=" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isEqual))

                "not=" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isNotEqual))

                ">" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isGreaterThan))

                ">=" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isGreaterThanOrEqual))

                "<" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isLessThan))

                "<=" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isLessThanOrEqual))

                "apply" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.apply_))

                "cons" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.cons))

                "first" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.first))

                "float?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isFloat))

                "get" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.get))

                "integer?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isInteger))

                "list" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.list))

                "Exception." ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.newException))

                "not" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.not_))

                "number?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isNumber))

                "rem" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.rem))

                "rest" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.rest_))

                "seq" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.seq))

                "sleep" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.sleep))

                "str" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.str))

                "throw" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.throw))

                _ ->
                    Err (Exception ("Unknown symbol " ++ symbol))
            )


{-| Introduce a redundant closure to prevent closure shadowing via tail-call optimization
in functions with continuations. See: <https://github.com/elm/compiler/issues/2017>

Elm compiler doesn’t seem to be able to optimize this away.

-}
closureHack3 : a -> b -> c -> (a -> b -> c -> d) -> d
closureHack3 a b c f =
    f a b c


evalExpression : Located Value -> Env -> Continuation -> Step
evalExpression mutableExpr mutableEnv mutableK =
    closureHack3
        mutableExpr
        mutableEnv
        mutableK
        (\(Located loc expr) env k ->
            case expr of
                Vector v ->
                    evalVector (Located loc v) env k

                Map m ->
                    evalMap (Located loc m) env k

                Set s ->
                    evalSet (Located loc s) env k

                Symbol s ->
                    case resolveSymbol env s of
                        Ok v ->
                            ( Ok ( Located loc (Const v), env ), Just (Thunk k) )

                        Err e ->
                            ( Err (Located loc e), Just (Thunk k) )

                String s ->
                    ( Ok ( Located loc (Const (String s)), env ), Just (Thunk k) )

                Keyword s ->
                    ( Ok ( Located loc (Const (Keyword s)), env ), Just (Thunk k) )

                Number (Float _) ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                Number (Int _) ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                MapEntry _ ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                Nil ->
                    ( Ok ( Located loc (Const Nil), env ), Just (Thunk k) )

                Bool _ ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                Ref _ _ ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                Fn _ _ ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                List l ->
                    case l of
                        -- special forms
                        (Located _ (Symbol "def")) :: args ->
                            evalDef (Located loc args) env k

                        (Located _ (Symbol "if")) :: args ->
                            evalIf (Located loc args) env k

                        (Located _ (Symbol "do")) :: exprs ->
                            evalDo (Located loc exprs) env k

                        (Located _ (Symbol "quote")) :: exprs ->
                            evalQuote (Located loc exprs) env k

                        (Located _ (Symbol "let")) :: exprs ->
                            evalLet (Located loc exprs) env k

                        (Located _ (Symbol "fn")) :: exprs ->
                            evalFn (Located loc exprs) env k

                        -- apply
                        fnExpr :: argExprs ->
                            evalApply fnExpr (Located loc argExprs) env k

                        -- empty list ()
                        [] ->
                            ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )

                Throwable _ ->
                    ( Ok ( Located loc (Const expr), env ), Just (Thunk k) )
        )


evalVector : Located (List (Located Value)) -> Env -> Continuation -> Step
evalVector (Located loc v) env k =
    case v of
        x :: rest ->
            evalExpression x
                env
                (\xret xenv ->
                    evalExpression
                        (Located loc (Vector rest))
                        xenv
                        (\(Located _ restRetVal) restEnv ->
                            case restRetVal of
                                Vector restRet ->
                                    ( Ok ( Located loc (Const (Vector (xret :: restRet))), restEnv )
                                    , Just (Thunk k)
                                    )

                                _ ->
                                    ( Err
                                        (Located loc
                                            (Exception
                                                "Impossible interpreter state: list evaluation yielded a non-list"
                                            )
                                        )
                                    , Just (Thunk k)
                                    )
                        )
                )

        [] ->
            ( Ok ( Located loc (Const (Vector [])), env ), Just (Thunk k) )


evalMap : Located Enclojure.Types.ValueMap -> Env -> Continuation -> Step
evalMap (Located mapLoc map) env k =
    ( Ok ( Located mapLoc (Const (Map ValueMap.empty)), env )
    , map
        |> ValueMap.toList
        |> List.map (Located mapLoc)
        |> List.foldl
            (\e a ->
                \(Located loc v) renv ->
                    case v of
                        Map m ->
                            evalMapEntry e
                                renv
                                (\(Located mapEntryLoc mapEntryV) mapEntryEnv ->
                                    case mapEntryV of
                                        MapEntry ( key, value ) ->
                                            ( Ok
                                                ( Located mapEntryLoc (Const (Map (ValueMap.insert key value m)))
                                                , mapEntryEnv
                                                )
                                            , Just (Thunk a)
                                            )

                                        _ ->
                                            ( "Interpreter error: Map entry evaluation yielded a non-map entry"
                                                |> Exception
                                                |> Located loc
                                                |> Err
                                            , Just (Thunk a)
                                            )
                                )

                        _ ->
                            ( "Interpreter error: Map evaluation yielded a non-map"
                                |> Exception
                                |> Located loc
                                |> Err
                            , Just (Thunk a)
                            )
            )
            k
        |> Thunk
        |> Just
    )


evalMapEntry : Located Enclojure.Types.ValueMapEntry -> Env -> Continuation -> Step
evalMapEntry (Located loc ( key, value )) env k =
    evalExpression (Located Enclojure.Types.fakeLoc key)
        env
        (\keyRet keyEnv ->
            evalExpression value
                keyEnv
                (\valRet valEnv ->
                    ( Ok ( Located loc (Const (MapEntry ( Located.getValue keyRet, valRet ))), valEnv )
                    , Just (Thunk k)
                    )
                )
        )


evalSet : Located Enclojure.Types.ValueSet -> Env -> Continuation -> Step
evalSet (Located setLoc set) env k =
    ( Ok ( Located setLoc (Const (Set ValueSet.empty)), env )
    , set
        |> ValueSet.toList
        |> List.map (Located setLoc)
        |> List.foldl
            (\e a ->
                \(Located loc v) renv ->
                    case v of
                        Set s ->
                            evalExpression e
                                renv
                                (\(Located setEntryLoc setEntry) setEntryEnv ->
                                    ( Ok
                                        ( Located setEntryLoc (Const (Set (ValueSet.insert setEntry s)))
                                        , setEntryEnv
                                        )
                                    , Just (Thunk a)
                                    )
                                )

                        _ ->
                            ( "Interpreter error: Set evaluation yielded a non-set"
                                |> Exception
                                |> Located loc
                                |> Err
                            , Just (Thunk a)
                            )
            )
            k
        |> Thunk
        |> Just
    )


mapArgs : List (Located Value) -> List (Located Value) -> Result (Located Exception) (List ( String, Value ))
mapArgs args bindings =
    case ( args, bindings ) of
        ( _, (Located _ (Symbol "&")) :: (Located _ (Symbol name)) :: [] ) ->
            Ok [ ( name, List args ) ]

        ( _, (Located loc (Symbol "&")) :: [] ) ->
            Err (Located loc (Exception "Parsing error: no symbol after &"))

        ( (Located loc _) :: _, [] ) ->
            Err (Located loc (Exception "Argument error: Too many arguments"))

        ( [], (Located loc (Symbol _)) :: _ ) ->
            Err (Located loc (Exception "Argument error: Too few arguments"))

        ( (Located _ arg) :: restArgs, (Located _ (Symbol name)) :: restBindings ) ->
            mapArgs restArgs restBindings |> Result.map (\b -> ( name, arg ) :: b)

        ( (Located loc _) :: _, _ ) ->
            Err (Located loc (Exception "Parsing error: arguments didn't match the function definition"))

        ( _, (Located loc _) :: _ ) ->
            Err (Located loc (Exception "Parsing error: arguments didn't match the function definition"))

        ( [], [] ) ->
            Ok []


bindArgs : Located Value -> List (Located Value) -> Env -> Result (Located Exception) Env
bindArgs (Located loc argsExpr) bindings env =
    case argsExpr of
        List args ->
            mapArgs args bindings
                |> Result.map
                    (List.foldl (\( k, v ) aEnv -> Runtime.setLocalEnv k v aEnv) env)

        _ ->
            Err (Located loc (Exception "Interpreter error: applied arguments are not a list"))


mapBindingsToBodies : List (Located Value) -> Result (Located Exception) (List ( List (Located Value), Located Value ))
mapBindingsToBodies signatures =
    case signatures of
        (Located _ (List ((Located loc (Vector argBindings)) :: body))) :: rest ->
            mapBindingsToBodies rest |> Result.map (\r -> ( argBindings, wrapInDo (Located loc body) ) :: r)

        (Located loc _) :: _ ->
            Err (Located loc (Exception "Parsing error: malformed function arity"))

        [] ->
            Ok []


listLocate : (a -> Maybe b) -> List a -> Maybe b
listLocate pFn l =
    case l of
        [] ->
            Nothing

        e :: rest ->
            case pFn e of
                Just v ->
                    Just v

                Nothing ->
                    listLocate pFn rest


exctractFnName : List (Located Value) -> ( Maybe String, List (Located Value) )
exctractFnName exprs =
    case exprs of
        (Located _ (Symbol name)) :: rest ->
            ( Just name, rest )

        _ ->
            ( Nothing, exprs )


evalFn : Located (List (Located Value)) -> Env -> Continuation -> Step
evalFn (Located loc exprs) fnEnv k =
    let
        ( name, arity ) =
            exctractFnName exprs
    in
    case arity of
        (Located _ (Vector argBindings)) :: body ->
            ( Ok
                ( Fn name
                    (\fn ->
                        (\args callsiteEnv ->
                            let
                                callEnvResult =
                                    { fnEnv | global = callsiteEnv.global }
                                        |> (name
                                                |> Maybe.map (\n -> Runtime.setLocalEnv n fn.self)
                                                |> Maybe.withDefault identity
                                           )
                                        |> Runtime.setLocalEnv "recur" fn.self
                                        |> bindArgs args argBindings
                            in
                            case callEnvResult of
                                Ok callEnv ->
                                    evalExpression (wrapInDo (Located loc body))
                                        callEnv
                                        (scrubLocalEnv callsiteEnv fn.k)

                                Err e ->
                                    ( Err e, Just (Thunk fn.k) )
                        )
                            |> Thunk
                    )
                    |> Const
                    |> Located loc
                , fnEnv
                )
            , Just (Thunk k)
            )

        signatures ->
            ( ( (\fn ->
                    (\args callsiteEnv ->
                        let
                            callEnvBodyResult =
                                { fnEnv | global = callsiteEnv.global }
                                    |> (name
                                            |> Maybe.map (\n -> Runtime.setLocalEnv n fn.self)
                                            |> Maybe.withDefault identity
                                       )
                                    |> Runtime.setLocalEnv "recur" fn.self
                                    |> (\env ->
                                            mapBindingsToBodies signatures
                                                |> Result.andThen
                                                    (listLocate
                                                        (\( bindings, body ) ->
                                                            bindArgs args bindings env
                                                                |> Result.toMaybe
                                                                |> Maybe.map (\e -> ( e, body ))
                                                        )
                                                        >> Result.fromMaybe
                                                            (Located loc
                                                                (Exception
                                                                    "Argument error: no matching arity"
                                                                )
                                                            )
                                                    )
                                       )
                        in
                        case callEnvBodyResult of
                            Ok ( callEnv, fnBody ) ->
                                evalExpression fnBody
                                    callEnv
                                    (scrubLocalEnv callsiteEnv fn.k)

                            Err e ->
                                ( Err e, Just (Thunk fn.k) )
                    )
                        |> Thunk
                )
                    |> Fn name
                    |> Const
                    |> Located loc
              , fnEnv
              )
                |> Ok
            , Just (Thunk k)
            )


scrubLocalEnv : Env -> Continuation -> Continuation
scrubLocalEnv priorEnv k =
    \v env ->
        ( Ok ( Located.map Const v, { env | local = priorEnv.local } ), Just (Thunk k) )


evalLet : Located (List (Located Value)) -> Env -> Continuation -> Step
evalLet (Located loc body) env k =
    let
        parseBindings bindings =
            case bindings of
                (Located _ (Symbol name)) :: value :: rest ->
                    parseBindings rest
                        |> Result.map (\r -> ( name, value ) :: r)

                [] ->
                    Ok []

                _ ->
                    Err (Located loc (Exception "Syntax error: let received an uneven number of binding pairs"))
    in
    case body of
        (Located bodyLoc (Vector bindings)) :: doBody ->
            ( Ok ( Located loc (Const Nil), env )
            , Just
                (bindings
                    |> parseBindings
                    |> Result.map
                        (List.foldr
                            (\( name, e ) a ->
                                \_ eenv ->
                                    evalExpression e
                                        eenv
                                        (\ret retEnv ->
                                            a
                                                (Located loc Nil)
                                                (Runtime.setLocalEnv name (Located.getValue ret) retEnv)
                                        )
                            )
                            (\_ renv ->
                                wrapInDo (Located bodyLoc doBody)
                                    |> (\b -> evalExpression b renv (scrubLocalEnv env k))
                            )
                        )
                    |> Result.map Thunk
                    |> (\r ->
                            case r of
                                Ok v ->
                                    v

                                Err e ->
                                    Thunk (\_ _ -> ( Err e, Just (Thunk (scrubLocalEnv env k)) ))
                       )
                )
            )

        _ ->
            ( Err (Located loc (Exception "Syntax error: let expects a vector of bindings")), Just (Thunk k) )


evalApply : Located Value -> Located (List (Located Value)) -> Env -> Continuation -> Step
evalApply fnExpr (Located loc argExprs) env k =
    evalExpression fnExpr
        env
        (\fn fnEnv ->
            ( Ok ( Located loc (Const (List [])), fnEnv )
            , Just
                (argExprs
                    |> List.foldr
                        (\e a ->
                            \(Located _ args) argsEnv ->
                                evalExpression e
                                    argsEnv
                                    (\arg argEnv ->
                                        case args of
                                            List ea ->
                                                a (Located loc (List (ea ++ [ arg ]))) argEnv

                                            _ ->
                                                ( "Impossible interpreter state: list evaluation yielded a non-list"
                                                    |> Exception
                                                    |> Located loc
                                                    |> Err
                                                , Just (Thunk a)
                                                )
                                    )
                        )
                        (\args argsEnv -> Lib.apply fn args argsEnv k)
                    |> Thunk
                )
            )
        )


evalQuote : Located (List (Located Value)) -> Env -> Continuation -> Step
evalQuote (Located loc exprs) env k =
    case exprs of
        [ arg ] ->
            ( Ok ( Located.map Const arg, env )
            , Just (Thunk k)
            )

        _ ->
            ( Err
                (Located loc
                    (Exception
                        ("Argument error: Wrong number of arguments ("
                            ++ String.fromInt (List.length exprs)
                            ++ ") passed to quote"
                        )
                    )
                )
            , Just (Thunk k)
            )


evalDo : Located (List (Located Value)) -> Env -> Continuation -> Step
evalDo (Located loc exprs) env k =
    ( Ok ( Located loc (Const Nil), env )
    , Just
        (Thunk
            (exprs
                |> List.foldr
                    (\e a -> \_ eenv -> evalExpression e eenv a)
                    (\r renv -> ( Ok ( Located.map Const r, renv ), Just (Thunk k) ))
            )
        )
    )


evalIf : Located (List (Located Value)) -> Env -> Continuation -> Step
evalIf (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ :: _ ->
            ( Err (Located loc (Exception "an if with too many forms")), Just (Thunk k) )

        [ eIf, eThen, eElse ] ->
            evalExpression eIf
                env
                (\ifRet ifEnv ->
                    if Runtime.isTruthy (Located.getValue ifRet) then
                        evalExpression eThen ifEnv k

                    else
                        evalExpression eElse ifEnv k
                )

        [ eIf, eThen ] ->
            evalExpression eIf
                env
                (\ifRet ifEnv ->
                    if Runtime.isTruthy (Located.getValue ifRet) then
                        evalExpression eThen ifEnv k

                    else
                        ( Ok ( Located loc (Const Nil), ifEnv ), Just (Thunk k) )
                )

        [ _ ] ->
            ( Err (Located loc (Exception "an if without then")), Just (Thunk k) )

        [] ->
            ( Err (Located loc (Exception "an empty if")), Just (Thunk k) )


evalDef : Located (List (Located Value)) -> Env -> Continuation -> Step
evalDef (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ ->
            ( Err (Located loc (Exception "too many arguments to def")), Just (Thunk k) )

        [ Located _ (Symbol name), e ] ->
            evalExpression e
                env
                (\eret eenv ->
                    ( Ok
                        ( Located loc (Const (Ref name eret))
                        , Runtime.setGlobalEnv name (Located.getValue eret) eenv
                        )
                    , Just (Thunk k)
                    )
                )

        [ _, _ ] ->
            ( Err (Located loc (Exception "def accepts a symbol and an expression")), Just (Thunk k) )

        [ Located _ (Symbol name) ] ->
            ( Err (Located loc (Exception ("empty def " ++ name))), Just (Thunk k) )

        [ _ ] ->
            ( Err (Located loc (Exception "def expects a symbol as its first argument")), Just (Thunk k) )

        [] ->
            ( Err (Located loc (Exception "no arguments to def")), Just (Thunk k) )


wrapInDo : Located (List (Located Value)) -> Located Value
wrapInDo (Located loc vs) =
    Located loc (List (Located loc (Symbol "do") :: vs))


prelude : Result (List Parser.DeadEnd) (List (Located Value))
prelude =
    Parser.parse Lib.prelude


eval : String -> Step
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.map2
            (\a b -> a ++ b)
            (prelude |> Result.mapError (always (Exception "Interpreter error: failed to load the prelude")))
        |> Result.andThen
            (\exprs ->
                exprs
                    |> List.head
                    |> Maybe.map ((\lv -> Located.replace lv exprs) >> wrapInDo)
                    |> Result.fromMaybe (Exception "Empty program")
            )
        |> Result.map
            (\program ->
                evalExpression program
                    Runtime.emptyEnv
                    (\(Located pos v) env ->
                        ( Ok ( Located pos (Const v), env ), Nothing )
                    )
            )
        |> (\r ->
                case r of
                    Ok v ->
                        v

                    Err e ->
                        ( Err (Located { start = ( 0, 0 ), end = ( 0, 0 ) } e)
                        , Nothing
                        )
           )
