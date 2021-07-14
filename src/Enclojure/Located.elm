module Enclojure.Located exposing (Located(..), getValue, map, replace)


type alias Position =
    { start : ( Int, Int ), end : ( Int, Int ) }


type Located a
    = Located Position a


replace : Located a -> b -> Located b
replace (Located pos _) val =
    Located pos val


map : (a -> b) -> Located a -> Located b
map f (Located pos a) =
    Located pos (f a)


getValue : Located a -> a
getValue (Located _ val) =
    val
