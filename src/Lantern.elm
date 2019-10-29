module Lantern exposing (RequestPort, Response, ResponsePort)

import Json.Decode
import Json.Encode


type alias RequestPort msg =
    String -> Cmd msg


type alias ResponsePort msg =
    (String -> msg) -> Sub msg


type alias Response =
    String
