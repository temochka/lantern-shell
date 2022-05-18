module Lantern.Request exposing (Id, Request(..))

import Lantern.Http as Http
import Lantern.Query as Query


type alias Id =
    String


type Request
    = Nop
    | Echo String
    | ReaderQuery Query.Query
    | WriterQuery Query.Query
    | LiveQuery (List (List Query.Query))
    | Migration Query.Query
    | HttpRequest Http.RequestPayload
