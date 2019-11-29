module Lantern.Response exposing (..)

import Json.Decode
import Lantern.Query
import Lantern.Request


type Response
    = Echo String
    | ReaderQuery Lantern.Query.ReaderResult
    | WriterQuery Lantern.Query.WriterResult
    | LiveQuery (List (List Lantern.Query.ReaderResult))
    | Migration
    | Unknown Json.Decode.Value
