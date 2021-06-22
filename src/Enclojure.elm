module Enclojure exposing (eval)

import Enclojure.Extra.Maybe
import Enclojure.Lib as Lib
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Parser as Parser
import Enclojure.Runtime as Runtime exposing (Arity(..), Exception(..), Value(..))


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

        _ ->
            Err (Exception <| "Unknown symbol " ++ symbol)


apply : Located Value -> List (Located Value) -> Result (Located Exception) (Located Value)
apply ((Located _ value) as fn) args =
    case value of
        Fn _ callable ->
            let
                result =
                    Runtime.invoke callable (List.map Located.getValue args)
            in
            result
                |> Result.map (\ret -> Located.replace fn ret)
                |> Result.mapError (\e -> Located.replace fn e)

        _ ->
            Err (Located.replace fn (Exception <| Runtime.inspectLocated fn ++ " is not a callable"))


evalExpression : Located Parser.Expr -> Result (Located Exception) (Located Value)
evalExpression ((Located _ value) as expr) =
    case value of
        Parser.List expressions ->
            let
                results =
                    expressions
                        |> List.map evalExpression
                        |> List.foldr (Result.map2 (::)) (Ok [])
            in
            case results of
                Ok evaledExpressions ->
                    case evaledExpressions of
                        [] ->
                            Ok (Located.replace expr (List []))

                        fn :: xs ->
                            apply fn xs

                Err exception ->
                    Err exception

        Parser.Symbol symbol ->
            resolveSymbol symbol
                |> Result.map (Located.replace expr)
                |> Result.mapError (Located.replace expr)

        Parser.Number number ->
            Ok (Located.replace expr (Number number))

        Parser.Nil ->
            Ok (Located.replace expr Runtime.Nil)

        Parser.Do exprs ->
            List.foldl
                (\e _ ->
                    evalExpression e
                )
                (Ok (Located.replace expr Nil))
                exprs


eval : String -> Result String String
eval code =
    Parser.parse code
        |> Result.map (evalExpression >> Debug.toString)
        |> Result.mapError Debug.toString
