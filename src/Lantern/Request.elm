module Lantern.Request exposing (..)

import Json.Encode
import Lantern.Query as Query


type alias Id =
    String


type Request
    = Echo String
    | Query Query.Query
    | LiveQuery (List Query.Query)
    | Migration Query.Query
