module Enclojure exposing (eval, evalPure, init, terminate)

import Array exposing (Array)
import Enclojure.Extra.Maybe exposing (orElse)
import Enclojure.Lib as Lib
import Enclojure.Lib.String as LibString
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Parser
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Arity(..), Continuation, Env, Exception(..), IO(..), Number(..), Step, Thunk(..), Value(..))
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Parser


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
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.apply))

                "assoc" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.assoc))

                "conj" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.conj))

                "cons" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.cons))

                "contains?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.contains))

                "dissoc" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.dissoc))

                "first" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.first))

                "float?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isFloat))

                "get" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.get))

                "json/encode" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.jsonEncode))

                "json/decode" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.jsonDecode))

                "http/request" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.http))

                "integer?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isInteger))

                "key" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.key_))

                "list" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.list))

                "Exception." ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.newException))

                "not" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.not_))

                "number?" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.isNumber))

                "peek" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.peek))

                "pr-str" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.prStr))

                "read-field" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.readField))

                "rem" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.rem))

                "rest" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.rest_))

                "second" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.second))

                "seq" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.seq))

                "sleep" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.sleep))

                "str" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.str))

                "string/join" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation LibString.join))

                "string/length" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation LibString.length))

                "string/split-lines" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation LibString.splitLines))

                "throw" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.throw))

                "ui" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.ui))

                "val" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.val_))

                "<o>" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.savepoint))

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
                            ( Err ( Located loc e, env ), Just (Thunk k) )

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

                        (Located _ (Symbol "do")) :: exprs ->
                            evalDo (Located loc exprs) env k

                        (Located _ (Symbol "if")) :: args ->
                            evalIf (Located loc args) env k

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


evalVector : Located (Array (Located Value)) -> Env -> Continuation -> Step
evalVector (Located loc vecV) env k =
    ( Ok ( Located loc (Const (Vector Array.empty)), env )
    , vecV
        |> Array.foldr
            (\e a ->
                \(Located _ v) envNow ->
                    case v of
                        Vector array ->
                            evalExpression e
                                envNow
                                (\ret retEnv ->
                                    ( Ok ( Located loc (Const (Vector (Array.push ret array))), retEnv )
                                    , Just (Thunk a)
                                    )
                                )

                        _ ->
                            ( Err ( Located loc (Exception "Interpreter error: vector evaluation yielded a non-vector"), envNow )
                            , Just (Thunk a)
                            )
            )
            k
        |> Thunk
        |> Just
    )


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
                                                |> (\ex -> ( ex, mapEntryEnv ))
                                                |> Err
                                            , Just (Thunk a)
                                            )
                                )

                        _ ->
                            ( "Interpreter error: Map evaluation yielded a non-map"
                                |> Exception
                                |> Located loc
                                |> (\ex -> ( ex, renv ))
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
                                |> (\ex -> ( ex, renv ))
                                |> Err
                            , Just (Thunk a)
                            )
            )
            k
        |> Thunk
        |> Just
    )


destructure : Located Value -> Located Value -> Result (Located Exception) (List ( String, Value ))
destructure arg template =
    case template of
        Located _ (Symbol name) ->
            Ok [ ( name, Located.getValue arg ) ]

        Located loc (Map templateMap) ->
            arg
                |> Located.getValue
                |> Runtime.toMap
                |> Result.fromMaybe (Located loc (Exception "type error: destructured value is not associative"))
                |> Result.andThen
                    (\map ->
                        let
                            keywordKeys =
                                templateMap
                                    |> ValueMap.get (Keyword "keys")
                                    |> Maybe.map Located.getValue
                                    |> Maybe.withDefault (List [])
                                    |> Runtime.trySequenceOf Runtime.trySymbol
                                    |> Result.fromMaybe (Located loc (Exception "type error: :keys must be a vector of symbols"))
                                    |> Result.map
                                        (\keys ->
                                            keys
                                                |> List.map
                                                    (\key ->
                                                        ( key
                                                        , ValueMap.get (Keyword key) map
                                                            |> Maybe.map Located.getValue
                                                            |> Maybe.withDefault Nil
                                                        )
                                                    )
                                        )

                            stringKeys =
                                templateMap
                                    |> ValueMap.get (Keyword "strs")
                                    |> Maybe.map Located.getValue
                                    |> Maybe.withDefault (List [])
                                    |> Runtime.trySequenceOf Runtime.trySymbol
                                    |> Result.fromMaybe (Located loc (Exception "type error: :strs must be a vector of symbols"))
                                    |> Result.map
                                        (\keys ->
                                            keys
                                                |> List.map
                                                    (\key ->
                                                        ( key
                                                        , ValueMap.get (String key) map
                                                            |> Maybe.map Located.getValue
                                                            |> Maybe.withDefault Nil
                                                        )
                                                    )
                                        )

                            otherKeys =
                                templateMap
                                    |> ValueMap.remove (Keyword "keys")
                                    |> ValueMap.remove (Keyword "strs")
                                    |> ValueMap.remove (Keyword "or")
                                    |> ValueMap.toList
                                    |> List.foldr
                                        (\( keyTemplate, Located _ key ) allBindingsResult ->
                                            allBindingsResult
                                                |> Result.andThen
                                                    (\allBindings ->
                                                        destructure
                                                            (ValueMap.get key map
                                                                |> Maybe.withDefault (Located loc Nil)
                                                            )
                                                            (Located loc keyTemplate)
                                                            |> Result.map ((++) allBindings)
                                                    )
                                        )
                                        (Ok [])

                            defaults =
                                if ValueMap.member (Keyword "or") templateMap then
                                    Err (Located loc (Exception ":or is not supported"))

                                else
                                    Ok []
                        in
                        Result.map4 (\a b c d -> a ++ b ++ c ++ d)
                            defaults
                            keywordKeys
                            stringKeys
                            otherKeys
                    )

        Located loc (Vector aliasedVector) ->
            let
                asKeyword =
                    Array.get (Array.length aliasedVector - 2) aliasedVector
                        |> Maybe.andThen (Located.getValue >> Runtime.tryKeyword)

                asTemplate =
                    Array.get (Array.length aliasedVector - 1) aliasedVector
                        |> Maybe.andThen (Located.getValue >> Runtime.trySymbol)

                ( vec, asBindings ) =
                    Maybe.map2
                        (\_ tmpl ->
                            ( Array.slice 0 -2 aliasedVector, [ ( tmpl, Located.getValue arg ) ] )
                        )
                        asKeyword
                        asTemplate
                        |> Maybe.withDefault ( aliasedVector, [] )
            in
            case Array.toList vec of
                [] ->
                    Ok asBindings

                [ Located _ (Symbol "&"), nextTemplate ] ->
                    arg
                        |> Located.getValue
                        |> Runtime.toSeq
                        |> Result.mapError (Located loc)
                        |> Result.andThen
                            (\seq ->
                                destructure (Located.sameAs arg (List seq)) nextTemplate
                            )
                        |> Result.map ((++) asBindings)

                nextTemplate :: rest ->
                    arg
                        |> Located.getValue
                        |> Runtime.toSeq
                        |> Result.mapError (Located loc)
                        |> Result.andThen
                            (\argAsList ->
                                let
                                    newArg =
                                        argAsList |> List.head |> Maybe.withDefault (Located.fakeLoc Nil)
                                in
                                destructure newArg nextTemplate
                                    |> Result.andThen
                                        (\boundArgs ->
                                            destructure
                                                (argAsList
                                                    |> List.tail
                                                    |> Maybe.map List
                                                    |> Maybe.withDefault Nil
                                                    |> Located.fakeLoc
                                                )
                                                (Located loc (Vector (Array.fromList rest)))
                                                |> Result.map ((++) asBindings)
                                                |> Result.map ((++) boundArgs)
                                        )
                            )

        _ ->
            Err (Located.sameAs arg (Exception "Parsing error: arguments didn't match the function definition"))


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

        ( arg :: restArgs, template :: restBindings ) ->
            destructure arg template
                |> Result.andThen
                    (\destructuredArgs ->
                        mapArgs restArgs restBindings |> Result.map (\b -> destructuredArgs ++ b)
                    )

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
            mapBindingsToBodies rest |> Result.map (\r -> ( Array.toList argBindings, wrapInDo (Located loc body) ) :: r)

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
                                        |> bindArgs args (Array.toList argBindings)
                            in
                            case callEnvResult of
                                Ok callEnv ->
                                    evalExpression (wrapInDo (Located loc body))
                                        callEnv
                                        (scrubLocalEnv callsiteEnv fn.k)

                                Err e ->
                                    ( Err ( e, fnEnv ), Just (Thunk fn.k) )
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
                                ( Err ( e, callsiteEnv ), Just (Thunk fn.k) )
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
                template :: value :: rest ->
                    parseBindings rest
                        |> Result.map (\r -> ( template, value ) :: r)

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
                    |> Array.toList
                    |> parseBindings
                    |> Result.map
                        (List.foldr
                            (\( template, e ) a ->
                                \_ eenv ->
                                    evalExpression e
                                        eenv
                                        (\ret retEnv ->
                                            ( destructure ret template
                                                |> Result.mapError (\ex -> ( ex, retEnv ))
                                                |> Result.andThen
                                                    (\destructuredBindings ->
                                                        Ok
                                                            ( Located loc (Const Nil)
                                                            , destructuredBindings
                                                                |> List.foldl (\( name, v ) -> Runtime.setLocalEnv name v) retEnv
                                                            )
                                                    )
                                            , Just (Thunk a)
                                            )
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
                                    Thunk (\_ errEnv -> ( Err ( e, errEnv ), Just (Thunk (scrubLocalEnv env k)) ))
                       )
                )
            )

        _ ->
            ( Err ( Located loc (Exception "Syntax error: let expects a vector of bindings"), env ), Just (Thunk k) )


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
                                                    |> (\ex -> ( ex, argEnv ))
                                                    |> Err
                                                , Just (Thunk a)
                                                )
                                    )
                        )
                        (\args argsEnv -> Runtime.apply fn args argsEnv k)
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
                ( Located loc
                    (Exception
                        ("Argument error: Wrong number of arguments ("
                            ++ String.fromInt (List.length exprs)
                            ++ ") passed to quote"
                        )
                    )
                , env
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
            ( Err ( Located loc (Exception "an if with too many forms"), env ), Just (Thunk k) )

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
            ( Err ( Located loc (Exception "an if without then"), env ), Just (Thunk k) )

        [] ->
            ( Err ( Located loc (Exception "an empty if"), env ), Just (Thunk k) )


evalDef : Located (List (Located Value)) -> Env -> Continuation -> Step
evalDef (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ ->
            ( Err ( Located loc (Exception "too many arguments to def"), env ), Just (Thunk k) )

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
            ( Err ( Located loc (Exception "def accepts a symbol and an expression"), env ), Just (Thunk k) )

        [ Located _ (Symbol name) ] ->
            ( Err ( Located loc (Exception ("empty def " ++ name)), env ), Just (Thunk k) )

        [ _ ] ->
            ( Err ( Located loc (Exception "def expects a symbol as its first argument"), env ), Just (Thunk k) )

        [] ->
            ( Err ( Located loc (Exception "no arguments to def"), env ), Just (Thunk k) )


wrapInDo : Located (List (Located Value)) -> Located Value
wrapInDo (Located loc vs) =
    Located loc (List (Located loc (Symbol "do") :: vs))


prelude : Result (List Parser.DeadEnd) (List (Located Value))
prelude =
    Parser.parse Lib.prelude


terminate : Continuation
terminate (Located pos v) env =
    ( Ok ( Located pos (Const v), env ), Nothing )


trampolinePure : ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk ) -> Result (Located Exception) ( Value, Env )
trampolinePure ( result, thunk ) =
    case result of
        Ok ( io, env ) ->
            case Located.getValue io of
                Const v ->
                    case thunk of
                        Just (Thunk continuation) ->
                            trampolinePure (continuation (Located.sameAs io v) env)

                        Nothing ->
                            Ok ( v, env )

                _ ->
                    Err (Located.sameAs io (Exception "Interpreter error: An unexpected side effect during init"))

        Err ( e, _ ) ->
            Err e


init : Env
init =
    prelude
        |> Result.map (Located.fakeLoc >> wrapInDo)
        |> Result.map (\program -> evalExpression program Runtime.emptyEnv terminate)
        |> Result.mapError (always (Located.fakeLoc (Exception "Interpreter error: Dead end")))
        |> Result.andThen trampolinePure
        |> Result.map (\( _, env ) -> env)
        |> Result.withDefault Runtime.emptyEnv


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Parser.Problem -> String
problemToString p =
    case p of
        Parser.Expecting s ->
            "expecting '" ++ s ++ "'"

        Parser.ExpectingInt ->
            "expecting int"

        Parser.ExpectingHex ->
            "expecting hex"

        Parser.ExpectingOctal ->
            "expecting octal"

        Parser.ExpectingBinary ->
            "expecting binary"

        Parser.ExpectingFloat ->
            "expecting float"

        Parser.ExpectingNumber ->
            "expecting number"

        Parser.ExpectingVariable ->
            "expecting variable"

        Parser.ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        Parser.ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        Parser.ExpectingEnd ->
            "expecting end"

        Parser.UnexpectedChar ->
            "unexpected char"

        Parser.Problem s ->
            s

        Parser.BadRepeat ->
            "bad repeat"


evalPure : Env -> String -> Result (Located Exception) ( Value, Env )
evalPure initEnv code =
    Parser.parse code
        |> Result.mapError (deadEndsToString >> Exception)
        |> Result.andThen
            (\exprs ->
                exprs
                    |> List.head
                    |> Maybe.map ((\lv -> Located.sameAs lv exprs) >> wrapInDo)
                    |> Result.fromMaybe (Exception "Empty program")
            )
        |> Result.map
            (\program ->
                evalExpression program
                    initEnv
                    terminate
            )
        |> Result.mapError Located.fakeLoc
        |> Result.andThen trampolinePure


eval : Env -> String -> Step
eval initEnv code =
    Parser.parse code
        |> Result.mapError (deadEndsToString >> Exception)
        |> Result.map2
            (\a b -> a ++ b)
            (prelude |> Result.mapError (always (Exception "Interpreter error: failed to load the prelude")))
        |> Result.andThen
            (\exprs ->
                exprs
                    |> List.head
                    |> Maybe.map ((\lv -> Located.sameAs lv exprs) >> wrapInDo)
                    |> Result.fromMaybe (Exception "Empty program")
            )
        |> Result.map
            (\program ->
                evalExpression program
                    initEnv
                    terminate
            )
        |> (\r ->
                case r of
                    Ok v ->
                        v

                    Err e ->
                        ( Err ( Located { start = ( 0, 0 ), end = ( 0, 0 ) } e, initEnv )
                        , Nothing
                        )
           )
