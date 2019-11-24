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


logResponse : Log -> Lantern.Request.Id -> Lantern.Response.Response -> Log
logResponse currentLog requestId response =
    case response of
        Lantern.Response.Echo text ->
            log currentLog Info ("[#" ++ requestId ++ "] Echo response: " ++ text)

        Lantern.Response.ReaderQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Json.Encode.encode 0 result)

        Lantern.Response.WriterQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Debug.toString result)

        Lantern.Response.LiveQuery result ->
            log currentLog Info ("[#" ++ requestId ++ "] Query response: " ++ Json.Encode.encode 0 (Json.Encode.list identity result))

        Lantern.Response.Migration ->
            log currentLog Info ("[#" ++ requestId ++ "] Migration response: ack")

        Lantern.Response.Unknown payload ->
            log currentLog Error ("[#" ++ requestId ++ "] Unknown resposne: " ++ Json.Encode.encode 0 payload)


log : Log -> Level -> String -> Log
log currentLog level string =
    { currentLog | lines = ( level, string ) :: currentLog.lines }
