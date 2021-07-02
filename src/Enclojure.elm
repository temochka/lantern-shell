module Enclojure exposing (Continuation, Thunk(..), eval)

import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Parser as Parser
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


evalExpression : Located Parser.Expr -> Thunk -> ( Result (Located Exception) (Located IO), Maybe Thunk )
evalExpression mutableExpr mutableK =
    closureHack2
        mutableExpr
        mutableK
        (\(Located loc expr) k ->
            case expr of
                Parser.Apply e1 e2 ->
                    evalExpression e1
                        (Thunk
                            (\e1ret ->
                                evalExpression e2
                                    (Thunk
                                        (\e2ret ->
                                            ( apply e1ret e2ret, Just k )
                                        )
                                    )
                            )
                        )

                Parser.Def name e ->
                    evalExpression e
                        (Thunk
                            (\eret ->
                                ( Ok (Located loc (Const (Ref name eret))), Just k )
                            )
                        )

                Parser.List l ->
                    case l of
                        x :: rest ->
                            evalExpression x
                                (Thunk
                                    (\xret ->
                                        evalExpression
                                            (Located loc (Parser.List rest))
                                            (Thunk
                                                (\(Located _ restRetVal) ->
                                                    case restRetVal of
                                                        List restRet ->
                                                            ( Ok (Located loc (Const (List (xret :: restRet)))), Just k )

                                                        _ ->
                                                            ( Err (Located loc (Exception "Impossible interpreter state: list evaluation yielded a non-list")), Just k )
                                                )
                                            )
                                    )
                                )

                        [] ->
                            ( Ok (Located loc (Const (List []))), Just k )

                Parser.Conditional { ifExpression, thenExpression, elseExpression } ->
                    evalExpression ifExpression
                        (Thunk
                            (\ifRet ->
                                if Runtime.isTruthy (Located.getValue ifRet) then
                                    evalExpression thenExpression k

                                else
                                    elseExpression
                                        |> Maybe.map (\e -> evalExpression e k)
                                        |> Maybe.withDefault ( Ok (Located loc (Const Nil)), Just k )
                            )
                        )

                Parser.Do l ->
                    case l of
                        [ x ] ->
                            evalExpression x k

                        x :: rest ->
                            evalExpression x
                                (Thunk
                                    (\_ ->
                                        evalExpression
                                            (Located loc
                                                (Parser.Do rest)
                                            )
                                            k
                                    )
                                )

                        [] ->
                            ( Ok (Located loc (Const Nil)), Just k )

                Parser.Symbol s ->
                    case resolveSymbol s of
                        Ok v ->
                            ( Ok (Located loc (Const v)), Just k )

                        Err e ->
                            ( Err (Located loc e), Just k )

                Parser.Number n ->
                    ( Ok (Located loc (Const (Number n))), Just k )

                Parser.Nil ->
                    ( Ok (Located loc (Const Nil)), Just k )

                Parser.Bool b ->
                    ( Ok (Located loc (Const (Bool b))), Just k )
        )


eval : String -> ( Result (Located Exception) (Located IO), Maybe Thunk )
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.map
            (\expr ->
                evalExpression expr
                    (Thunk
                        (\(Located pos v) ->
                            ( Ok (Located pos (Const v)), Nothing )
                        )
                    )
            )
        |> Result.withDefault ( Err (Located { start = ( 0, 0 ), end = ( 0, 0 ) } (Exception "Parsing error")), Nothing )
