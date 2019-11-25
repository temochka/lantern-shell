module DevTools.FlexiQuery exposing (Result, resultDecoder)

import Dict exposing (Dict)
import Json.Decode
import Lantern.Query


type alias Result =
    Dict String Lantern.Query.Value


resultDecoder : Json.Decode.Decoder Result
resultDecoder =
    Json.Decode.dict Lantern.Query.valueDecoder
