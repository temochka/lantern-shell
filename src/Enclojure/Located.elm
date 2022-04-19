module Enclojure.Located exposing (Located(..), fakeLoc, getPos, getValue, map, sameAs)


type alias Position =
    { start : ( Int, Int ), end : ( Int, Int ) }


type Located a
    = Located Position a


sameAs : Located a -> b -> Located b
sameAs (Located pos _) val =
    Located pos val


map : (a -> b) -> Located a -> Located b
map f (Located pos a) =
    Located pos (f a)


getValue : Located a -> a
getValue (Located _ val) =
    val


getPos : Located a -> Position
getPos (Located pos _) =
    pos


fakeLoc : a -> Located a
fakeLoc v =
    Located { start = ( 0, 0 ), end = ( 0, 0 ) } v
