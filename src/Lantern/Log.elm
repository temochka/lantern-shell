module Lantern.Log exposing (Level(..), Log, log, logRequest, logResponse, new)

import Json.Encode
import Lantern.Request
import Lantern.Response


type Level
    = Debug
    | Info
    | Error


type alias Line =
    ( Level, String )


type alias Log =
    { lines : List Line
    , maxLines : Int
    }


new : Log
new =
    { lines = []
    , maxLines = 200
    }


logRequest : Log -> Lantern.Request.Id -> Lantern.Request.Request -> Log
logRequest currentLog requestId request =
    case request of
        Lantern.Request.Nop ->
            log currentLog Info ("[#" ++ requestId ++ "] Nop request")

        Lantern.Request.Echo text ->
            log currentLog Info ("[#" ++ requestId ++ "] Echo request: " ++ text)

        Lantern.Request.ReaderQuery { source, arguments } ->
            log currentLog Info ("[#" ++ requestId ++ "] Query request: " ++ source ++ "(" ++ Debug.toString arguments ++ ")")

        Lantern.Request.WriterQuery { source, arguments } ->
            log currentLog Info ("[#" ++ requestId ++ "] Query request: " ++ source ++ "(" ++ Debug.toString arguments ++ ")")

        Lantern.Request.LiveQuery queries ->
            log currentLog Info ("[#" ++ requestId ++ "] Live query request: " ++ Debug.toString queries)

        Lantern.Request.Migration { source } ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration request: " ++ source)

        Lantern.Request.HttpRequest httpRequest ->
            log currentLog Info ("[#" ++ requestId ++ "] HTTP request: " ++ Debug.toString httpRequest)


logResponse : Log -> Lantern.Request.Id -> Lantern.Response.Response -> Log
logResponse currentLog requestId response =
    case response of
        Lantern.Response.Nop ->
            log currentLog Info ("[#" ++ requestId ++ "] Server acked the nop")

        Lantern.Response.Hello ->
            log currentLog Info ("[#" ++ requestId ++ "] Server says hello")

        Lantern.Response.Echo text ->
            log currentLog Info ("[#" ++ requestId ++ "] Echo response: " ++ text)

        Lantern.Response.ReaderQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Json.Encode.encode 0 result)

        Lantern.Response.WriterQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Debug.toString result)

        Lantern.Response.LiveQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Debug.toString result)

        Lantern.Response.Migration ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration response: ack")

        Lantern.Response.HttpRequest httpResponse ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration response: " ++ Debug.toString httpResponse)

        Lantern.Response.Unknown payload ->
            log currentLog Error ("[#" ++ requestId ++ "] Unknown response: " ++ Json.Encode.encode 0 payload)

        Lantern.Response.FatalError error ->
            log currentLog Error ("[#" ++ requestId ++ "] Fatal server error:" ++ error)


log : Log -> Level -> String -> Log
log currentLog level string =
    { currentLog | lines = ( level, string ) :: currentLog.lines }
