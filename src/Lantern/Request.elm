module Lantern.Request exposing (..)

import Json.Encode
import Lantern.Query as Query


type alias Id =
    String


type Request
    = Echo String
    | ReaderQuery Query.Query
    | WriterQuery Query.Query
    | LiveQuery (List (List Query.Query))
    | Migration Query.Query
