module Lantern.Response exposing (..)

import Json.Decode
import Lantern.Query
import Lantern.Request


type Response
    = Echo String
    | Query Lantern.Query.SelectResult
    | LiveQuery (List Lantern.Query.SelectResult)
    | Migration
    | Unknown Json.Decode.Value
