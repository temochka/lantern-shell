module Lantern.Log exposing (Level(..), Log, log, logRequest, logResponse, new)

import Json.Decode
import Json.Encode
import Lantern.Encoders
import Lantern.Query
import Lantern.Request
import Lantern.Response


type Level
    = Debug
    | Info
    | Error


type alias Line =
    { id : Int
    , level : Level
    , text : String
    , payload : Maybe String
    }


type alias Log =
    { lines : List Line
    , lastId : Int
    , maxLines : Int
    }


new : Log
new =
    { lines = []
    , lastId = 0
    , maxLines = 200
    }


logRequest : Log -> Lantern.Request.Id -> Lantern.Request.Request -> Log
logRequest currentLog requestId request =
    let
        formattedRequestPayload =
            Lantern.Encoders.request requestId request |> Json.Encode.encode 2 |> Just
    in
    case request of
        Lantern.Request.Nop ->
            log currentLog Info ("[#" ++ requestId ++ "] Nop request") formattedRequestPayload

        Lantern.Request.Echo _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Echo request") formattedRequestPayload

        Lantern.Request.ReaderQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Reader request") formattedRequestPayload

        Lantern.Request.WriterQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Writer request") formattedRequestPayload

        Lantern.Request.LiveQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Live query request") formattedRequestPayload

        Lantern.Request.Migration _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration request") formattedRequestPayload

        Lantern.Request.HttpRequest _ ->
            log currentLog Info ("[#" ++ requestId ++ "] HTTP request") formattedRequestPayload


logResponse : Log -> Lantern.Request.Id -> Lantern.Response.Response -> String -> Log
logResponse currentLog requestId response rawResponse =
    let
        formattedResponsePayload =
            rawResponse
                |> Json.Decode.decodeString Json.Decode.value
                |> Result.map (Json.Encode.encode 2)
                |> Result.toMaybe
    in
    case response of
        Lantern.Response.Nop ->
            log currentLog Info ("[#" ++ requestId ++ "] Nop response") formattedResponsePayload

        Lantern.Response.Hello ->
            log currentLog Info ("[#" ++ requestId ++ "] Server says hello") formattedResponsePayload

        Lantern.Response.Echo _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Echo response") formattedResponsePayload

        Lantern.Response.ReaderQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Reader response") formattedResponsePayload

        Lantern.Response.WriterQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Writer response") formattedResponsePayload

        Lantern.Response.LiveQuery _ ->
            log currentLog Info ("[#" ++ requestId ++ "] Live query response") formattedResponsePayload

        Lantern.Response.Migration ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration response") formattedResponsePayload

        Lantern.Response.HttpRequest _ ->
            log currentLog Info ("[#" ++ requestId ++ "] HTTP response") formattedResponsePayload

        Lantern.Response.Unknown _ ->
            log currentLog Error ("[#" ++ requestId ++ "] Unknown response") formattedResponsePayload

        Lantern.Response.FatalError _ ->
            log currentLog Error ("[#" ++ requestId ++ "] Fatal server error") formattedResponsePayload


log : Log -> Level -> String -> Maybe String -> Log
log currentLog level text payload =
    let
        id =
            currentLog.lastId + 1

        newLine =
            { id = id, text = text, level = level, payload = payload }
    in
    { currentLog | lastId = id, lines = newLine :: currentLog.lines }
