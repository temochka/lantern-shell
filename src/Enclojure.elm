module Enclojure exposing (Continuation, Thunk(..), eval)

import Dict
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Parser as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Exception(..), IO(..), Value(..))



-- type alias IO =
--     Enclojure.IO.IO
-- type Continuation
--     = Pure (Runtime.Env -> Result Exception ( Runtime.Env, InterpreterState ))
--     | IO (Runtime.Env -> Result Exception ( Runtime.Env, I ))
-- type alias Model =
--     { env : Runtime.Env
--     , code : String
--     }
-- type Step
--     = Step (Runtime.Env -> Value -> ( Runtime.Env, Result Exception IO ))
-- resolveSymbol : String -> Result Exception Value
-- resolveSymbol symbol =
--     case symbol of
--         "+" ->
--             Ok (Fn (Just symbol) Lib.plus)
--         "-" ->
--             Ok (Fn (Just symbol) Lib.minus)
--         "/" ->
--             Ok (Fn (Just symbol) Lib.div)
--         "*" ->
--             Ok (Fn (Just symbol) Lib.mul)
--         -- "sleep" ->
--         --     Ok (Fn (Just symbol) Lib.sleep)
--         _ ->
--             Err (Exception <| "Unknown symbol " ++ symbol)


resolveSymbol : String -> Value
resolveSymbol symbol =
    case symbol of
        "+" ->
            Fn (Just symbol) Lib.plus

        "-" ->
            Fn (Just symbol) Lib.minus

        "/" ->
            Fn (Just symbol) Lib.div

        "*" ->
            Fn (Just symbol) Lib.mul

        "sleep" ->
            Fn (Just symbol) Lib.sleep

        _ ->
            Nil



-- apply : Located Value -> List (Located Value) -> Result (Located Exception) (Located Value)
-- apply ((Located _ value) as fn) args =
--     case value of
--         Fn _ callable ->
--             let
--                 result =
--                     Runtime.invoke callable (List.map Located.getValue args)
--             in
--             result
--                 |> Result.map (\ret -> Located.replace fn ret)
--                 |> Result.mapError (\e -> Located.replace fn e)
--         _ ->
--             Err (Located.replace fn (Exception <| Runtime.inspectLocated fn ++ " is not a callable"))


apply : Located Value -> Located Value -> Located IO
apply (Located fnLoc fnExpr) (Located _ argsExpr) =
    case ( fnExpr, argsExpr ) of
        ( Fn _ callable, List args ) ->
            Located fnLoc (Runtime.invoke callable (List.map Located.getValue args))

        _ ->
            Located fnLoc (Const Nil)



-- type Continuation
--     = Continuation (Located Value -> Result (Located Exception) ( IO, Maybe Continuation ))


type alias Continuation =
    Located Value -> Located Value


type Thunk
    = Thunk (Located Value -> ( Located IO, Maybe Thunk ))


{-| Introduce a redundant closure to prevent closure shadowing via tail-call optimization
in functions with continuations. See: <https://github.com/elm/compiler/issues/2017>

Elm compiler doesn’t seem to be able to optimize this away.

-}
closureHack2 : a -> b -> (a -> b -> c) -> c
closureHack2 a b f =
    f a b


evalExpression : Located Parser.Expr -> Thunk -> ( Located IO, Maybe Thunk )
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
                                                            ( Located loc (Const (List (xret :: restRet))), Just k )

                                                        _ ->
                                                            ( Located loc (Const (List [ xret ])), Just k )
                                                )
                                            )
                                    )
                                )

                        [] ->
                            ( Located loc (Const (List [])), Just k )

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
                            ( Located loc (Const Nil), Just k )

                Parser.Symbol s ->
                    ( Located loc (Const (resolveSymbol s)), Just k )

                Parser.Number n ->
                    ( Located loc (Const (Number n)), Just k )

                Parser.Nil ->
                    ( Located loc (Const Nil), Just k )
        )


eval : String -> ( Located IO, Maybe Thunk )
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.map
            (\expr ->
                evalExpression expr
                    (Thunk
                        (\(Located pos v) ->
                            ( Located pos (Const v), Nothing )
                        )
                    )
            )
        |> Result.withDefault ( Located { start = ( 0, 0 ), end = ( 0, 0 ) } (Const Runtime.Nil), Nothing )
