module Lantern.Extra.Result exposing (map10, map6, map7, map8, map9)


map6 :
    (a -> b -> c -> d -> e -> f -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x f
    -> Result x value
map6 fn resultA resultB resultC resultD resultE resultF =
    resultA
        |> Result.andThen
            (\a ->
                resultB
                    |> Result.andThen
                        (\b ->
                            resultC
                                |> Result.andThen
                                    (\c ->
                                        resultD
                                            |> Result.andThen
                                                (\d ->
                                                    resultE
                                                        |> Result.andThen
                                                            (\e ->
                                                                resultF
                                                                    |> Result.map
                                                                        (\f -> fn a b c d e f)
                                                            )
                                                )
                                    )
                        )
            )


map7 :
    (a -> b -> c -> d -> e -> f -> g -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x f
    -> Result x g
    -> Result x value
map7 fn resultA resultB resultC resultD resultE resultF resultG =
    resultA
        |> Result.andThen
            (\a ->
                resultB
                    |> Result.andThen
                        (\b ->
                            resultC
                                |> Result.andThen
                                    (\c ->
                                        resultD
                                            |> Result.andThen
                                                (\d ->
                                                    resultE
                                                        |> Result.andThen
                                                            (\e ->
                                                                resultF
                                                                    |> Result.andThen
                                                                        (\f ->
                                                                            resultG
                                                                                |> Result.map
                                                                                    (\g ->
                                                                                        fn a b c d e f g
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x f
    -> Result x g
    -> Result x h
    -> Result x value
map8 fn resultA resultB resultC resultD resultE resultF resultG resultH =
    resultA
        |> Result.andThen
            (\a ->
                resultB
                    |> Result.andThen
                        (\b ->
                            resultC
                                |> Result.andThen
                                    (\c ->
                                        resultD
                                            |> Result.andThen
                                                (\d ->
                                                    resultE
                                                        |> Result.andThen
                                                            (\e ->
                                                                resultF
                                                                    |> Result.andThen
                                                                        (\f ->
                                                                            resultG
                                                                                |> Result.andThen
                                                                                    (\g ->
                                                                                        resultH
                                                                                            |> Result.map
                                                                                                (\h ->
                                                                                                    fn a b c d e f g h
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x f
    -> Result x g
    -> Result x h
    -> Result x i
    -> Result x value
map9 fn resultA resultB resultC resultD resultE resultF resultG resultH resultI =
    resultA
        |> Result.andThen
            (\a ->
                resultB
                    |> Result.andThen
                        (\b ->
                            resultC
                                |> Result.andThen
                                    (\c ->
                                        resultD
                                            |> Result.andThen
                                                (\d ->
                                                    resultE
                                                        |> Result.andThen
                                                            (\e ->
                                                                resultF
                                                                    |> Result.andThen
                                                                        (\f ->
                                                                            resultG
                                                                                |> Result.andThen
                                                                                    (\g ->
                                                                                        resultH
                                                                                            |> Result.andThen
                                                                                                (\h ->
                                                                                                    resultI
                                                                                                        |> Result.map
                                                                                                            (\i ->
                                                                                                                fn a b c d e f g h i
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


map10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> value)
    -> Result x a
    -> Result x b
    -> Result x c
    -> Result x d
    -> Result x e
    -> Result x f
    -> Result x g
    -> Result x h
    -> Result x i
    -> Result x j
    -> Result x value
map10 fn resultA resultB resultC resultD resultE resultF resultG resultH resultI resultJ =
    resultA
        |> Result.andThen
            (\a ->
                resultB
                    |> Result.andThen
                        (\b ->
                            resultC
                                |> Result.andThen
                                    (\c ->
                                        resultD
                                            |> Result.andThen
                                                (\d ->
                                                    resultE
                                                        |> Result.andThen
                                                            (\e ->
                                                                resultF
                                                                    |> Result.andThen
                                                                        (\f ->
                                                                            resultG
                                                                                |> Result.andThen
                                                                                    (\g ->
                                                                                        resultH
                                                                                            |> Result.andThen
                                                                                                (\h ->
                                                                                                    resultI
                                                                                                        |> Result.andThen
                                                                                                            (\i ->
                                                                                                                resultJ
                                                                                                                    |> Result.map
                                                                                                                        (\j ->
                                                                                                                            fn a b c d e f g h i j
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
