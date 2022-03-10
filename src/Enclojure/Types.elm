module Enclojure.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Enclojure.Located exposing (Located)
import Set


fakeLoc : { start : ( number, number ), end : ( number, number ) }
fakeLoc =
    { start = ( 0, 0 ), end = ( 0, 0 ) }


type Exception
    = Exception String


type alias Continuation =
    Located Value -> Env -> ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk )


type Thunk
    = Thunk Continuation


type alias Step =
    ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk )


type Arity a
    = Fixed (a -> Env -> Continuation -> ( Result ( Exception, Env ) ( IO, Env ), Maybe Thunk ))
    | Variadic ({ args : a, rest : List Value } -> Env -> Continuation -> ( Result ( Exception, Env ) ( IO, Env ), Maybe Thunk ))


type TextFormat
    = Plain String
    | TextRef InputKey


type InputCell
    = TextInput { suggestions : List String }
    | MaskedTextInput
    | Button { title : String }
    | Download { name : String, contentType : String, content : String }


type alias InputKey =
    String


type Cell
    = Input InputKey InputCell
    | Text (List TextFormat)
    | VStack (List Cell)
    | HStack (List Cell)


type alias UI =
    { state : ValueMap
    , cell : Cell
    , watchFn : Value
    }


type alias HttpRequest =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Maybe String
    }


type IO
    = Const Value
    | Savepoint Value
    | Http HttpRequest
    | Sleep Float
    | ReadField String
    | ShowUI UI


type alias Callable =
    { arity0 : Maybe (Arity ())
    , arity1 : Maybe (Arity Value)
    , arity2 : Maybe (Arity ( Value, Value ))
    , arity3 : Maybe (Arity ( Value, Value, Value ))
    }


type alias ValueMapEntry =
    ( Value, Located Value )


type alias ValueMap =
    { ints : Dict Int (Located Value)
    , floats : Dict Float (Located Value)
    , strings : Dict String (Located Value)
    , nil : Maybe (Located Value)
    , bools : { true : Maybe (Located Value), false : Maybe (Located Value) }
    , keywords : Dict String (Located Value)
    , symbols : Dict String (Located Value)
    , fns : List ( Value, Located Value )
    , maps : List ( Value, Located Value )
    , mapEntries : List ( Value, Located Value )
    , lists : List ( Value, Located Value )
    , refs : List ( Value, Located Value )
    , sets : List ( Value, Located Value )
    , throwables : List ( Value, Located Value )
    , vectors : List ( Value, Located Value )
    }


type alias ValueSet =
    { ints : Set.Set Int
    , floats : Set.Set Float
    , strings : Set.Set String
    , nil : Maybe Value
    , bools : { true : Bool, false : Bool }
    , keywords : Set.Set String
    , symbols : Set.Set String
    , fns : List Value
    , maps : List Value
    , mapEntries : List Value
    , lists : List Value
    , refs : List Value
    , sets : List Value
    , throwables : List Value
    , vectors : List Value
    }


type Number
    = Float Float
    | Int Int


type Value
    = Number Number
    | String String
    | Ref String (Located Value)
    | Fn (Maybe String) ({ self : Value, k : Continuation } -> Thunk)
    | List (List (Located Value))
    | Vector (Array (Located Value))
    | Nil
    | Bool Basics.Bool
    | Keyword String
    | Map ValueMap
    | MapEntry ValueMapEntry
    | Set ValueSet
    | Symbol String
    | Throwable Exception


type alias Env =
    { global : Dict String Value
    , local : Dict String Value
    }
