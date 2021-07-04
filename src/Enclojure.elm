module Enclojure exposing (Continuation, Thunk(..), eval)

import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Exception(..), IO(..), Value(..))


resolveSymbol : String -> Result Exception Value
resolveSymbol symbol =
    case symbol of
        "+" ->
            Ok (Fn (Just symbol) Lib.plus)

        "-" ->
            Ok (Fn (Just symbol) Lib.minus)

        "/" ->
            Ok (Fn (Just symbol) Lib.div)

        "*" ->
            Ok (Fn (Just symbol) Lib.mul)

        "sleep" ->
            Ok (Fn (Just symbol) Lib.sleep)

        _ ->
            Err (Exception ("Unknown symbol " ++ symbol))


apply : Located Value -> Located Value -> Result (Located Exception) (Located IO)
apply ((Located fnLoc fnExpr) as fn) ((Located _ argsExpr) as arg) =
    case ( fnExpr, argsExpr ) of
        ( Fn _ callable, List args ) ->
            Runtime.invoke callable (List.map Located.getValue args) |> Result.map (Located fnLoc) |> Result.mapError (Located fnLoc)

        ( _, List _ ) ->
            Err (Located fnLoc (Exception (Runtime.inspectLocated fn ++ " is not a valid callable.")))

        ( _, _ ) ->
            Err (Located fnLoc (Exception ("Cannot apply " ++ Runtime.inspectLocated fn ++ " to " ++ Runtime.inspectLocated arg)))


type alias Continuation =
    Located Value -> Result (Located Exception) (Located Value)


type Thunk
    = Thunk (Located Value -> ( Result (Located Exception) (Located IO), Maybe Thunk ))


{-| Introduce a redundant closure to prevent closure shadowing via tail-call optimization
in functions with continuations. See: <https://github.com/elm/compiler/issues/2017>

Elm compiler doesn’t seem to be able to optimize this away.

-}
closureHack2 : a -> b -> (a -> b -> c) -> c
closureHack2 a b f =
    f a b


evalExpression : Located Value -> Thunk -> ( Result (Located Exception) (Located IO), Maybe Thunk )
evalExpression mutableExpr mutableK =
    closureHack2
        mutableExpr
        mutableK
        (\(Located loc expr) k ->
            case expr of
                Vector l ->
                    case l of
                        x :: rest ->
                            evalExpression x
                                (Thunk
                                    (\xret ->
                                        evalExpression
                                            (Located loc (Vector rest))
                                            (Thunk
                                                (\(Located _ restRetVal) ->
                                                    case restRetVal of
                                                        Vector restRet ->
                                                            ( Ok (Located loc (Const (Vector (xret :: restRet)))), Just k )

                                                        _ ->
                                                            ( Err (Located loc (Exception "Impossible interpreter state: list evaluation yielded a non-list")), Just k )
                                                )
                                            )
                                    )
                                )

                        [] ->
                            ( Ok (Located loc (Const (Vector []))), Just k )

                Symbol s ->
                    case resolveSymbol s of
                        Ok v ->
                            ( Ok (Located loc (Const v)), Just k )

                        Err e ->
                            ( Err (Located loc e), Just k )

                Number n ->
                    ( Ok (Located loc (Const (Number n))), Just k )

                Nil ->
                    ( Ok (Located loc (Const Nil)), Just k )

                Bool _ ->
                    ( Ok (Located loc (Const expr)), Just k )

                Ref _ _ ->
                    ( Ok (Located loc (Const expr)), Just k )

                Fn _ _ ->
                    ( Ok (Located loc (Const expr)), Just k )

                List l ->
                    case l of
                        (Located _ (Symbol "def")) :: args ->
                            case args of
                                _ :: _ :: _ :: _ ->
                                    ( Err (Located loc (Exception "too many arguments to def")), Just k )

                                [ Located _ (Symbol name), e ] ->
                                    evalExpression e
                                        (Thunk
                                            (\eret ->
                                                ( Ok (Located loc (Const (Ref name eret))), Just k )
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
                                        (Thunk
                                            (\ifRet ->
                                                if Runtime.isTruthy (Located.getValue ifRet) then
                                                    evalExpression eThen k

                                                else
                                                    evalExpression eElse k
                                            )
                                        )

                                [ eIf, eThen ] ->
                                    evalExpression eIf
                                        (Thunk
                                            (\ifRet ->
                                                if Runtime.isTruthy (Located.getValue ifRet) then
                                                    evalExpression eThen k

                                                else
                                                    ( Ok (Located loc (Const Nil)), Just k )
                                            )
                                        )

                                [ _ ] ->
                                    ( Err (Located loc (Exception "an if without then")), Just k )

                                [] ->
                                    ( Err (Located loc (Exception "an empty if")), Just k )

                        (Located _ (Symbol "do")) :: exprs ->
                            ( Ok (Located loc (Const Nil))
                            , Just
                                (Thunk
                                    (exprs
                                        |> List.foldr
                                            (\e a -> \_ -> evalExpression e (Thunk a))
                                            (\r -> ( Ok (Located.map Const r), Just k ))
                                    )
                                )
                            )

                        (Located _ (Symbol "quot")) :: exprs ->
                            case exprs of
                                [ arg ] ->
                                    ( Ok (Located.map Const arg)
                                    , Just k
                                    )

                                _ ->
                                    ( Err (Located loc (Exception ("Wrong number of arguments (" ++ String.fromInt (List.length exprs) ++ ") passed to quot"))), Just k )

                        -- apply
                        fnExpr :: argExprs ->
                            evalExpression fnExpr
                                (Thunk
                                    (\fn ->
                                        ( Ok (Located loc (Const (List [])))
                                        , Just
                                            (Thunk
                                                (argExprs
                                                    |> List.foldr
                                                        (\e a ->
                                                            \(Located _ args) ->
                                                                evalExpression e
                                                                    (Thunk
                                                                        (\arg ->
                                                                            case args of
                                                                                List ea ->
                                                                                    ( Ok (Located loc (Const (List (arg :: ea))))
                                                                                    , Just (Thunk a)
                                                                                    )

                                                                                _ ->
                                                                                    ( Err (Located loc (Exception "Impossible interpreter state: list evaluation yielded a non-list"))
                                                                                    , Just (Thunk a)
                                                                                    )
                                                                        )
                                                                    )
                                                        )
                                                        (\args -> ( apply fn args, Just k ))
                                                )
                                            )
                                        )
                                    )
                                )

                        [] ->
                            ( Ok (Located loc (Const expr)), Just k )
        )


wrapInDo : List (Located Value) -> Maybe (Located Value)
wrapInDo vs =
    vs
        |> List.head
        |> Maybe.map
            (\loc ->
                Located.replace loc (List (Located.replace loc (Symbol "do") :: vs))
            )


eval : String -> ( Result (Located Exception) (Located IO), Maybe Thunk )
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.andThen (\exprs -> wrapInDo exprs |> Result.fromMaybe (Exception "Empty program"))
        |> Result.map
            (\program ->
                evalExpression program
                    (Thunk
                        (\(Located pos v) ->
                            ( Ok (Located pos (Const v)), Nothing )
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
