module Enclojure exposing (Continuation, eval)

import Dict
import Enclojure.IO as IO exposing (IO(..))
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Parser as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Exception(..), Value(..))



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

        -- "sleep" ->
        --     Ok (Fn (Just symbol) Lib.sleep)
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


apply : Located Value -> Located Value -> Located Value
apply (Located fnLoc fnExpr) (Located _ argsExpr) =
    -- let
    --     _ =
    --         Debug.log "expr: " ( fnExpr, argsExpr )
    -- in
    case ( fnExpr, argsExpr ) of
        ( Fn _ callable, List args ) ->
            Located fnLoc (Runtime.invoke callable (List.map Located.getValue args))

        _ ->
            Located fnLoc Nil



-- type Continuation
--     = Continuation (Located Value -> Result (Located Exception) ( IO, Maybe Continuation ))


type alias Continuation =
    Located Value -> Located Value



-- (+ 3 5)
-- AST (Apply (Symbol "+") [Num 3, Num 5])
-- Cont: (\x -> (5, (\val -> )))
-- Ok, the type is:
-- (IO, List Continuation) (or at least some sort of list of IO)
-- (do
--   (sleep (- 1000 (+ 1 2 3)))
--   (+ 1 1)
--
-- (() -> (
--    1000,
--    (v_1000) -> (
--      1,
--      (v_1) -> (
--        2,
--        (v_2) -> (
--          3,
--          (v_3) -> (
--
--          )
-- )
--      )
--      n ::
--    )
-- ))
-- evalList : List (Located Parser.Expr) -> ( Result (Located Exception) (List IO), Continuation )
-- evalList expressions =
--     case expressions of
--         current :: next :: rest ->
--             \locatedValue ->
--                 evalExpression current
--                     evalExpression
--                     next
-- evalExpression : Located Parser.Expr -> Result (Located Exception) IO
-- evalExpression ((Located _ value) as expr) =
--     case value of
--         Parser.List expressions ->
--             let
--                 results =
--                     expressions
--                         |> List.map evalExpression
--                         |> List.foldr (Result.map2 (::)) (Ok [])
--             in
--             case results of
--                 Ok evaledExpressions ->
--                     case evaledExpressions of
--                         [] ->
--                             Ok (Located.replace expr (List []))
--                         fn :: xs ->
--                             apply fn xs
--                 Err exception ->
--                     Err exception
--         Parser.Symbol symbol ->
--             resolveSymbol symbol
--                 |> Result.map (Located.replace expr >> Const)
--                 |> Result.mapError (Located.replace expr)
--         Parser.Number number ->
--             Number number
--                 |> Located.replace expr
--                 |> Const
--                 |> Ok
--         Parser.Nil ->
--             Ok (Const (Located.replace expr Runtime.Nil))
--         Parser.Do exprs ->
--             List.foldl
--                 (\e _ ->
--                     evalExpression e
--                 )
--                 (Ok (Located.replace expr Nil))
--                 exprs


evalExpression : Located Parser.Expr -> ( String, Continuation ) -> Located Value
evalExpression (Located loc expr) ( kname, k ) =
    let
        _ =
            Debug.log "expr, k" ( expr, kname )
    in
    case expr of
        Parser.Apply e1 e2 ->
            evalExpression e1
                ( "e1k"
                , \e1ret ->
                    evalExpression e2
                        ( "e2k"
                        , \e2ret ->
                            let
                                result =
                                    apply e1ret e2ret

                                -- _ =
                                --     Debug.log "(e1ret, e2ret, result)" ( e1ret, e2ret, result )
                            in
                            result
                        )
                )
                |> k

        Parser.List l ->
            case l of
                x :: rest ->
                    evalExpression x
                        ( "listx"
                        , \xret ->
                            evalExpression
                                (Located loc (Parser.List rest))
                                ( "listrest"
                                , \(Located _ restRetVal) ->
                                    case restRetVal of
                                        List restRet ->
                                            Located loc (List (xret :: restRet))

                                        v ->
                                            let
                                                _ =
                                                    Debug.log "impossible!" v
                                            in
                                            Located loc (List [ xret ])
                                )
                        )
                        |> k

                [] ->
                    Located loc (List []) |> k

        Parser.Symbol s ->
            Located loc (resolveSymbol s) |> k

        Parser.Number n ->
            Located loc (Number n) |> k

        Parser.Nil ->
            Located loc Nil |> k


eval : String -> Located Value
eval code =
    Parser.parse code
        |> Result.mapError (Debug.toString >> Exception)
        |> Result.map (\expr -> evalExpression expr ( "identity", identity ))
        |> Result.withDefault (Located { start = ( 0, 0 ), end = ( 0, 0 ) } Runtime.Nil)



-- step : Runtime.Env -> Continuation -> Result Runtime.Exception ( Runtime.Env, InterpreterState )
-- step env callback =
