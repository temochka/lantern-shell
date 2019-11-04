module Lantern exposing (RequestPort, Response, ResponsePort, State, initState, request)

import Json.Decode
import Json.Encode
import Lantern.Request


type alias RequestPort msg =
    String -> Cmd msg


type alias ResponsePort msg =
    (String -> msg) -> Sub msg


type alias Response =
    String


type alias State =
    { requestId : Int
    }


initState : State
initState =
    { requestId = 0 }


request : State -> RequestPort msg -> Lantern.Request.Request -> ( State, Cmd msg )
request state requestPort payload =
    let
        newState =
            { state | requestId = state.requestId + 1 }
    in
    ( newState, requestPort (Json.Encode.encode 0 (Lantern.Request.encode newState.requestId payload)) )
