module Enclojure.Located exposing (Located, replace)


type alias Located a =
    { value : a
    , startPosition : ( Int, Int )
    , endPosition : ( Int, Int )
    }


replace : Located a -> b -> Located b
replace { startPosition, endPosition } value =
    { value = value, startPosition = startPosition, endPosition = endPosition }
