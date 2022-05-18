module Lantern.Response exposing (Response(..))

import Json.Decode
import Lantern.Http as Http
import Lantern.Query


type Response
    = Hello
    | Nop
    | Echo String
    | ReaderQuery Lantern.Query.ReaderResult
    | WriterQuery Lantern.Query.WriterResult
    | LiveQuery (List (List Lantern.Query.ReaderResult))
    | HttpRequest Http.Response
    | Migration
    | FatalError String
    | Unknown Json.Decode.Value
