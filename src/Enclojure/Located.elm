module Enclojure.Located exposing (Located(..), getValue, replace)


type alias Position =
    { start : ( Int, Int ), end : ( Int, Int ) }


type Located a
    = Located Position a


replace : Located a -> b -> Located b
replace (Located pos _) val =
    Located pos val


getValue : Located a -> a
getValue (Located _ val) =
    val
