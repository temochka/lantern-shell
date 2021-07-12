module Enclojure.Reader.Macros exposing (macroexpandAll)

import Dict exposing (Dict)
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime exposing (Exception(..), Value(..))


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
                (Located _ (Symbol "__lambda")) :: args ->
                    expandLambda i (Located loc args)
                        |> Result.map Expanded

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


walk : a -> (a -> Located Value -> Result (Located Exception) ( a, Located Value )) -> Located Value -> Result (Located Exception) ( a, Located Value )
walk state f (Located loc val) =
    case val of
        List l ->
            l
                |> List.foldr (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState f e |> Result.map (\( nState, v ) -> ( nState, v :: av )))) (Ok ( state, [] ))
                |> Result.map (\( s, r ) -> ( s, Located loc (List r) ))

        Vector l ->
            l
                |> List.foldr (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState f e |> Result.map (\( nState, v ) -> ( nState, v :: av )))) (Ok ( state, [] ))
                |> Result.map (\( s, r ) -> ( s, Located loc (Vector r) ))

        _ ->
            f state (Located loc val)


type alias Arguments =
    { positional : Dict Int String
    , variadic : Maybe String
    }


expandLambda : Int -> Located (List (Located Value)) -> Result Exception ( Int, Located Value )
expandLambda i (Located loc body) =
    case body of
        [] ->
            Ok ( i, Located loc (List [ Located loc (Symbol "fn"), Located loc (Vector []), Located loc (List []) ]) )

        exprs ->
            case substituteLambdaArgs i exprs of
                Ok ( ( newI, args ), newExprs ) ->
                    let
                        ( finalI, completedArgs ) =
                            completeArguments newI args
                    in
                    Ok ( finalI, Located loc (List [ Located loc (Symbol "fn"), Located loc (argsToValue (Located loc completedArgs)), Located loc (List newExprs) ]) )

                Err e ->
                    Err (Located.getValue e)


completeArguments : Int -> Arguments -> ( Int, Arguments )
completeArguments startI arguments =
    let
        maxPositional =
            List.maximum (Dict.keys arguments.positional) |> Maybe.withDefault 0
    in
    List.range 1 maxPositional
        |> List.foldr
            (\e ( i, a ) ->
                let
                    ( newI, id ) =
                        a.positional
                            |> Dict.get e
                            |> Maybe.map (Tuple.pair i)
                            |> Maybe.withDefault ( i + 1, "p" ++ String.fromInt e ++ "__" ++ String.fromInt i )
                in
                ( newI, { a | positional = Dict.insert e id a.positional } )
            )
            ( startI, arguments )


argsToValue : Located Arguments -> Value
argsToValue (Located loc arguments) =
    let
        positional =
            arguments.positional
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map (Tuple.second >> Symbol >> Located loc)

        variadic =
            arguments.variadic |> Maybe.map (Symbol >> Located loc >> List.singleton >> (++) [ Located loc (Symbol "&") ]) |> Maybe.withDefault []
    in
    Vector (positional ++ variadic)


substituteLambdaArgsWalker : ( Int, Arguments ) -> Located Value -> Result (Located Exception) ( ( Int, Arguments ), Located Value )
substituteLambdaArgsWalker ( i, args ) (Located loc expr) =
    case expr of
        Symbol "__lambda" ->
            Err (Located loc (Exception "Parsing error: nested #() are not supported, use fn instead."))

        Symbol "%&" ->
            let
                ( newI, id ) =
                    args.variadic |> Maybe.map (Tuple.pair i) |> Maybe.withDefault ( i + 1, "rest__" ++ String.fromInt i )
            in
            Ok ( ( newI, { args | variadic = Just id } ), Located loc (Symbol id) )

        Symbol name ->
            let
                argN =
                    if name == "%" then
                        Just 1

                    else if String.startsWith "%" name then
                        String.toInt (String.dropLeft 1 name)
                            |> Maybe.andThen
                                (\n ->
                                    if n < 1 then
                                        Nothing

                                    else
                                        Just n
                                )

                    else
                        Nothing
            in
            case argN of
                Just n ->
                    let
                        ( newI, id ) =
                            Dict.get n args.positional |> Maybe.map (Tuple.pair i) |> Maybe.withDefault ( i + 1, "p" ++ String.fromInt n ++ "__" ++ String.fromInt i )
                    in
                    Ok ( ( newI, { args | positional = Dict.insert n id args.positional } ), Located loc (Symbol id) )

                Nothing ->
                    Ok ( ( i, args ), Located loc expr )

        _ ->
            Ok ( ( i, args ), Located loc expr )


substituteLambdaArgs : Int -> List (Located Value) -> Result (Located Exception) ( ( Int, Arguments ), List (Located Value) )
substituteLambdaArgs i exprs =
    let
        startState =
            ( i, { positional = Dict.empty, variadic = Nothing } )
    in
    exprs
        |> List.foldr
            (\e a -> a |> Result.andThen (\( aState, av ) -> walk aState substituteLambdaArgsWalker e |> Result.map (\( nState, v ) -> ( nState, v :: av ))))
            (Ok ( startState, [] ))
