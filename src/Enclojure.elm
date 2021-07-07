module Enclojure exposing (eval)

import Enclojure.Extra.Maybe exposing (orElse)
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Env, Exception(..), IO(..), Thunk(..), Value(..))


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

                "sleep" ->
                    Ok (Fn (Just symbol) (Runtime.toContinuation Lib.sleep))

                _ ->
                    Err (Exception ("Unknown symbol " ++ symbol))
            )


apply : Located Value -> Located Value -> Env -> Thunk -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
apply ((Located fnLoc fnExpr) as fn) arg env k =
    case fnExpr of
        Fn _ callable ->
            ( Ok ( Located.map Const arg, env ), Just (callable k) )

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

                Number n ->
                    ( Ok ( Located loc (Const (Number n)), env ), Just k )

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
                        (Located _ (Symbol "def")) :: args ->
                            case args of
                                _ :: _ :: _ :: _ ->
                                    ( Err (Located loc (Exception "too many arguments to def")), Just k )

                                [ Located _ (Symbol name), e ] ->
                                    evalExpression e
                                        env
                                        (Thunk
                                            (\eret eenv ->
                                                ( Ok ( Located loc (Const (Ref name eret)), { eenv | global = Runtime.setEnv name (Located.getValue eret) eenv.global } ), Just k )
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

                        (Located _ (Symbol "if")) :: args ->
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

                        (Located _ (Symbol "do")) :: exprs ->
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

                        (Located _ (Symbol "quote")) :: exprs ->
                            case exprs of
                                [ arg ] ->
                                    ( Ok ( Located.map Const arg, env )
                                    , Just k
                                    )

                                _ ->
                                    ( Err (Located loc (Exception ("Wrong number of arguments (" ++ String.fromInt (List.length exprs) ++ ") passed to quote"))), Just k )

                        -- apply
                        fnExpr :: argExprs ->
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
                                                                                    ( Ok ( Located loc (Const (List (arg :: ea))), argEnv )
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

                        [] ->
                            ( Ok ( Located loc (Const expr), env ), Just k )
        )


wrapInDo : List (Located Value) -> Maybe (Located Value)
wrapInDo vs =
    vs
        |> List.head
        |> Maybe.map
            (\loc ->
                Located.replace loc (List (Located.replace loc (Symbol "do") :: vs))
            )


eval : String -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.andThen (\exprs -> wrapInDo exprs |> Result.fromMaybe (Exception "Empty program"))
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
