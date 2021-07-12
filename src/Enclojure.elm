module Enclojure exposing (eval)

import Enclojure.Extra.Maybe exposing (orElse)
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Env, Exception(..), IO(..), Thunk(..), Value(..))
import Html exposing (a)


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

                "list" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.list))

                "not" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.not_))

                "sleep" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.sleep))

                _ ->
                    Err (Exception ("Unknown symbol " ++ symbol))
            )


apply : Located Value -> Located Value -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
apply ((Located fnLoc fnExpr) as fn) arg env k =
    case fnExpr of
        Fn _ callable ->
            ( Ok ( Located.map Const arg, env ), Just (callable { self = fnExpr, k = k }) )

        _ ->
            ( Err (Located fnLoc (Exception (Runtime.inspectLocated fn ++ " is not a valid callable."))), Just k )


{-| Introduce a redundant closure to prevent closure shadowing via tail-call optimization
in functions with continuations. See: <https://github.com/elm/compiler/issues/2017>

Elm compiler doesn’t seem to be able to optimize this away.

-}
closureHack3 : a -> b -> c -> (a -> b -> c -> d) -> d
closureHack3 a b c f =
    f a b c


evalExpression : Located Value -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalExpression mutableExpr mutableEnv mutableK =
    closureHack3
        mutableExpr
        mutableEnv
        mutableK
        (\(Located loc expr) env k ->
            case expr of
                Vector l ->
                    case l of
                        x :: rest ->
                            evalExpression
                                x
                                env
                                (Thunk
                                    (\xret xenv ->
                                        evalExpression
                                            (Located loc (Vector rest))
                                            xenv
                                            (Thunk
                                                (\(Located _ restRetVal) restEnv ->
                                                    case restRetVal of
                                                        Vector restRet ->
                                                            ( Ok ( Located loc (Const (Vector (xret :: restRet))), restEnv ), Just k )

                                                        _ ->
                                                            ( Err (Located loc (Exception "Impossible interpreter state: list evaluation yielded a non-list")), Just k )
                                                )
                                            )
                                    )
                                )

                        [] ->
                            ( Ok ( Located loc (Const (Vector [])), env ), Just k )

                Symbol s ->
                    case resolveSymbol env s of
                        Ok v ->
                            ( Ok ( Located loc (Const v), env ), Just k )

                        Err e ->
                            ( Err (Located loc e), Just k )

                Float n ->
                    ( Ok ( Located loc (Const (Float n)), env ), Just k )

                Int n ->
                    ( Ok ( Located loc (Const (Int n)), env ), Just k )

                Nil ->
                    ( Ok ( Located loc (Const Nil), env ), Just k )

                Bool _ ->
                    ( Ok ( Located loc (Const expr), env ), Just k )

                Ref _ _ ->
                    ( Ok ( Located loc (Const expr), env ), Just k )

                Fn _ _ ->
                    ( Ok ( Located loc (Const expr), env ), Just k )

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
                            ( Ok ( Located loc (Const expr), env ), Just k )
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
            mapArgs args bindings |> Result.map (\boundArgs -> boundArgs |> List.foldl (\( k, v ) aEnv -> Runtime.setLocalEnv k v aEnv) env)

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


evalFn : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalFn (Located loc exprs) fnEnv k =
    (case exprs of
        (Located _ (Symbol name)) :: rest ->
            ( Just name, rest )

        _ ->
            ( Nothing, exprs )
    )
        |> (\( name, arity ) ->
                case arity of
                    (Located _ (Vector argBindings)) :: body ->
                        ( Ok
                            ( Located loc <|
                                Const
                                    (Fn name
                                        (\fn ->
                                            Thunk
                                                (\args callsiteEnv ->
                                                    let
                                                        callEnvResult =
                                                            { fnEnv | global = callsiteEnv.global }
                                                                |> (name |> Maybe.map (\n -> Runtime.setLocalEnv n fn.self) |> Maybe.withDefault identity)
                                                                |> Runtime.setLocalEnv "recur" fn.self
                                                                |> bindArgs args argBindings
                                                    in
                                                    case callEnvResult of
                                                        Ok callEnv ->
                                                            evalExpression (wrapInDo (Located loc body))
                                                                callEnv
                                                                (scrubLocalEnv callsiteEnv fn.k)

                                                        Err e ->
                                                            ( Err e, Just fn.k )
                                                )
                                        )
                                    )
                            , fnEnv
                            )
                        , Just k
                        )

                    signatures ->
                        let
                            bindingsAndBodies =
                                mapBindingsToBodies signatures
                        in
                        ( Ok
                            ( Located loc <|
                                Const
                                    (Fn name
                                        (\fn ->
                                            Thunk
                                                (\args callsiteEnv ->
                                                    let
                                                        callEnvBodyResult =
                                                            { fnEnv | global = callsiteEnv.global }
                                                                |> (name |> Maybe.map (\n -> Runtime.setLocalEnv n fn.self) |> Maybe.withDefault identity)
                                                                |> Runtime.setLocalEnv "recur" fn.self
                                                                |> (\env ->
                                                                        bindingsAndBodies
                                                                            |> Result.andThen
                                                                                (listLocate
                                                                                    (\( bindings, body ) ->
                                                                                        bindArgs args bindings env
                                                                                            |> Result.toMaybe
                                                                                            |> Maybe.map (\e -> ( e, body ))
                                                                                    )
                                                                                    >> Result.fromMaybe (Located loc (Exception "Argument error: no matching arity"))
                                                                                )
                                                                   )
                                                    in
                                                    case callEnvBodyResult of
                                                        Ok ( callEnv, fnBody ) ->
                                                            evalExpression fnBody
                                                                callEnv
                                                                (scrubLocalEnv callsiteEnv fn.k)

                                                        Err e ->
                                                            ( Err e, Just fn.k )
                                                )
                                        )
                                    )
                            , fnEnv
                            )
                        , Just k
                        )
           )


scrubLocalEnv : Env -> Thunk -> Thunk
scrubLocalEnv priorEnv k =
    Thunk
        (\v env ->
            ( Ok ( Located.map Const v, { env | local = priorEnv.local } ), Just k )
        )


evalLet : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
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
                            (\( name, e ) a -> \_ eenv -> evalExpression e eenv (Thunk (\ret retEnv -> ( Ok ( Located loc (Const Nil), Runtime.setLocalEnv name (Located.getValue ret) retEnv ), Just (Thunk a) ))))
                            (\_ renv -> wrapInDo (Located bodyLoc doBody) |> (\b -> evalExpression b renv (scrubLocalEnv env k)))
                        )
                    |> Result.map Thunk
                    |> (\r ->
                            case r of
                                Ok v ->
                                    v

                                Err e ->
                                    Thunk (\_ _ -> ( Err e, Just (scrubLocalEnv env k) ))
                       )
                )
            )

        _ ->
            ( Err (Located loc (Exception "Syntax error: let expects a vector of bindings")), Just k )


evalApply : Located Value -> Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalApply fnExpr (Located loc argExprs) env k =
    evalExpression fnExpr
        env
        (Thunk
            (\fn fnEnv ->
                ( Ok ( Located loc (Const (List [])), fnEnv )
                , Just
                    (Thunk
                        (argExprs
                            |> List.foldr
                                (\e a ->
                                    \(Located _ args) argsEnv ->
                                        evalExpression e
                                            argsEnv
                                            (Thunk
                                                (\arg argEnv ->
                                                    case args of
                                                        List ea ->
                                                            ( Ok ( Located loc (Const (List (ea ++ [ arg ]))), argEnv )
                                                            , Just (Thunk a)
                                                            )

                                                        _ ->
                                                            ( Err (Located loc (Exception "Impossible interpreter state: list evaluation yielded a non-list"))
                                                            , Just (Thunk a)
                                                            )
                                                )
                                            )
                                )
                                (\args argsEnv -> apply fn args argsEnv k)
                        )
                    )
                )
            )
        )


evalQuote : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalQuote (Located loc exprs) env k =
    case exprs of
        [ arg ] ->
            ( Ok ( Located.map Const arg, env )
            , Just k
            )

        _ ->
            ( Err (Located loc (Exception ("Argument error: Wrong number of arguments (" ++ String.fromInt (List.length exprs) ++ ") passed to quote"))), Just k )


evalDo : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalDo (Located loc exprs) env k =
    ( Ok ( Located loc (Const Nil), env )
    , Just
        (Thunk
            (exprs
                |> List.foldr
                    (\e a -> \_ eenv -> evalExpression e eenv (Thunk a))
                    (\r renv -> ( Ok ( Located.map Const r, renv ), Just k ))
            )
        )
    )


evalIf : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalIf (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ :: _ ->
            ( Err (Located loc (Exception "an if with too many forms")), Just k )

        [ eIf, eThen, eElse ] ->
            evalExpression eIf
                env
                (Thunk
                    (\ifRet ifEnv ->
                        if Runtime.isTruthy (Located.getValue ifRet) then
                            evalExpression eThen ifEnv k

                        else
                            evalExpression eElse ifEnv k
                    )
                )

        [ eIf, eThen ] ->
            evalExpression eIf
                env
                (Thunk
                    (\ifRet ifEnv ->
                        if Runtime.isTruthy (Located.getValue ifRet) then
                            evalExpression eThen ifEnv k

                        else
                            ( Ok ( Located loc (Const Nil), ifEnv ), Just k )
                    )
                )

        [ _ ] ->
            ( Err (Located loc (Exception "an if without then")), Just k )

        [] ->
            ( Err (Located loc (Exception "an empty if")), Just k )


evalDef : Located (List (Located Value)) -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
evalDef (Located loc args) env k =
    case args of
        _ :: _ :: _ :: _ ->
            ( Err (Located loc (Exception "too many arguments to def")), Just k )

        [ Located _ (Symbol name), e ] ->
            evalExpression e
                env
                (Thunk
                    (\eret eenv ->
                        ( Ok ( Located loc (Const (Ref name eret)), Runtime.setGlobalEnv name (Located.getValue eret) eenv ), Just k )
                    )
                )

        [ _, _ ] ->
            ( Err (Located loc (Exception "def accepts a symbol and an expression")), Just k )

        [ Located _ (Symbol name) ] ->
            ( Err (Located loc (Exception ("empty def " ++ name))), Just k )

        [ _ ] ->
            ( Err (Located loc (Exception "def expects a symbol as its first argument")), Just k )

        [] ->
            ( Err (Located loc (Exception "no arguments to def")), Just k )


wrapInDo : Located (List (Located Value)) -> Located Value
wrapInDo (Located loc vs) =
    Located loc (List (Located loc (Symbol "do") :: vs))


eval : String -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.andThen (\exprs -> exprs |> List.head |> Maybe.map ((\lv -> Located.replace lv exprs) >> wrapInDo) |> Result.fromMaybe (Exception "Empty program"))
        |> Result.map
            (\program ->
                evalExpression program
                    Runtime.emptyEnv
                    (Thunk
                        (\(Located pos v) env ->
                            ( Ok ( Located pos (Const v), env ), Nothing )
                        )
                    )
            )
        |> (\r ->
                case r of
                    Ok v ->
                        v

                    Err e ->
                        ( Err (Located { start = ( 0, 0 ), end = ( 0, 0 ) } e), Nothing )
           )
