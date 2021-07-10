module Enclojure.Reader.Macros exposing (macroexpandAll)

import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime exposing (Exception(..), Value(..))
import Html exposing (i)


type Expansion a
    = Expanded a
    | Returned a


macroexpandAll : Located Value -> Result Exception (Located Value)
macroexpandAll v =
    macroexpandAllInternal 0 v |> Result.map Tuple.second


macroexpandAllInternal : Int -> Located Value -> Result Exception ( Int, Located Value )
macroexpandAllInternal i v =
    case macroexpand i v of
        Ok result ->
            case result of
                Expanded ( nextI, val ) ->
                    macroexpandAllInternal nextI val

                Returned ( nextI, Located loc val ) ->
                    case val of
                        List l ->
                            List.foldr
                                (\e a -> a |> Result.andThen (\( ni, lr ) -> macroexpandAllInternal ni e |> Result.map (\( nni, r ) -> ( nni, r :: lr ))))
                                (Ok ( nextI, [] ))
                                l
                                |> Result.map (Tuple.mapSecond (List >> Located loc))

                        Vector l ->
                            List.foldr
                                (\e a -> a |> Result.andThen (\( ni, lr ) -> macroexpandAllInternal ni e |> Result.map (\( nni, r ) -> ( nni, r :: lr ))))
                                (Ok ( nextI, [] ))
                                l
                                |> Result.map (Tuple.mapSecond (Vector >> Located loc))

                        _ ->
                            Ok ( nextI, v )

        Err e ->
            Err e


macroexpand : Int -> Located Value -> Result Exception (Expansion ( Int, Located Value ))
macroexpand i (Located loc value) =
    case value of
        List l ->
            case l of
                (Located _ (Symbol "and")) :: args ->
                    expandAnd i (Located loc args)
                        |> Result.map Expanded

                _ ->
                    Ok (Returned ( i, Located loc value ))

        _ ->
            Ok (Returned ( i, Located loc value ))


expandAnd : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandAnd i (Located loc args) =
    case args of
        (Located _ form) :: rest ->
            let
                id =
                    "and__" ++ String.fromInt i ++ "__auto__"
            in
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc (Vector [ Located loc (Symbol id), Located loc form ])
                        , Located loc
                            (List
                                [ Located loc (Symbol "if")
                                , Located loc (Symbol id)
                                , Located loc (List (Located loc (Symbol "and") :: rest))
                                , Located loc (Symbol id)
                                ]
                            )
                        ]
                    )
                )

        [] ->
            Ok ( i, Located loc (Bool True) )
