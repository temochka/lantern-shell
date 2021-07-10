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
                                (\e a ->
                                    a
                                        |> Result.andThen
                                            (\( ni, lr ) ->
                                                macroexpandAllInternal ni e |> Result.map (\( nni, r ) -> ( nni, r :: lr ))
                                            )
                                )
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
                            Ok ( nextI, Located loc val )

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

                (Located _ (Symbol "or")) :: args ->
                    expandOr i (Located loc args)
                        |> Result.map Expanded

                (Located _ (Symbol "when")) :: args ->
                    expandWhen i (Located loc args)
                        |> Result.map Expanded

                (Located _ (Symbol "when-not")) :: args ->
                    expandWhenNot i (Located loc args)
                        |> Result.map Expanded

                (Located _ (Symbol "->")) :: args ->
                    expandThreadFirst i (Located loc args)
                        |> Result.map Expanded

                (Located _ (Symbol "->>")) :: args ->
                    expandThreadLast i (Located loc args)
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
                ( i + 1
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


expandOr : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandOr i (Located loc args) =
    case args of
        (Located _ form) :: rest ->
            let
                id =
                    "or__" ++ String.fromInt i ++ "__auto__"
            in
            Ok <|
                ( i + 1
                , Located loc
                    (List
                        [ Located loc (Symbol "let")
                        , Located loc (Vector [ Located loc (Symbol id), Located loc form ])
                        , Located loc
                            (List
                                [ Located loc (Symbol "if")
                                , Located loc (Symbol id)
                                , Located loc (Symbol id)
                                , Located loc (List (Located loc (Symbol "or") :: rest))
                                ]
                            )
                        ]
                    )
                )

        [] ->
            Ok ( i, Located loc Nil )


expandWhen : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandWhen i (Located loc args) =
    case args of
        cond :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "if")
                        , cond
                        , Located loc
                            (List
                                (Located loc (Symbol "do") :: rest)
                            )
                        ]
                    )
                )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to when")


expandWhenNot : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandWhenNot i (Located loc args) =
    case args of
        cond :: rest ->
            Ok <|
                ( i
                , Located loc
                    (List
                        [ Located loc (Symbol "if")
                        , Located loc (List [ Located loc (Symbol "not"), cond ])
                        , Located loc
                            (List
                                (Located loc (Symbol "do") :: rest)
                            )
                        ]
                    )
                )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to when-not")


expandThreadFirst : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandThreadFirst i (Located loc args) =
    case args of
        arg :: op :: rest ->
            Ok
                ( i
                , Located loc
                    (List
                        (Located loc (Symbol "->")
                            :: (case op of
                                    Located _ (List forms) ->
                                        (\fn restArgs ->
                                            Located.replace fn (List (fn :: arg :: restArgs))
                                        )
                                            (List.head forms |> Maybe.withDefault (Located loc Nil))
                                            (List.tail forms |> Maybe.withDefault [])

                                    _ ->
                                        Located.replace arg (List [ op, arg ])
                               )
                            :: rest
                        )
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to ->")


expandThreadLast : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandThreadLast i (Located loc args) =
    case args of
        arg :: op :: rest ->
            Ok
                ( i
                , Located loc
                    (List
                        (Located loc (Symbol "->>")
                            :: (case op of
                                    Located lloc (List forms) ->
                                        Located lloc (List (forms ++ [ arg ]))

                                    _ ->
                                        Located.replace arg (List [ op, arg ])
                               )
                            :: rest
                        )
                    )
                )

        [ arg ] ->
            Ok ( i, arg )

        [] ->
            Err (Exception "Argument error: wrong number of arguments (0) passed to ->>")
