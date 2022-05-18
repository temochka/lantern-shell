module Enclojure.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Enclojure.Located as Located exposing (Located)
import Set


type Exception
    = Exception String (List StackFrame)


type alias Step io =
    ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) )


type alias Continuation io =
    Located (Value io) -> Env io -> Located (Step io)


type Thunk io
    = Thunk (Continuation io)


type Arity io a
    = Fixed (a -> Env io -> Continuation io -> ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) ))
    | Variadic ({ args : a, rest : List (Value io) } -> Env io -> Continuation io -> ( Result ( Exception, Env io ) ( IO io, Env io ), Maybe (Thunk io) ))


type IO io
    = Const (Value io)
    | SideEffect io


type alias Callable io =
    { arity0 : Maybe (Arity io ())
    , arity1 : Maybe (Arity io (Value io))
    , arity2 : Maybe (Arity io ( Value io, Value io ))
    , arity3 : Maybe (Arity io ( Value io, Value io, Value io ))
    }


type alias ValueMapEntry io =
    ( Value io, Located (Value io) )


type alias ValueMap io =
    { ints : Dict Int (Located (Value io))
    , floats : Dict Float (Located (Value io))
    , strings : Dict String (Located (Value io))
    , nil : Maybe (Located (Value io))
    , bools : { true : Maybe (Located (Value io)), false : Maybe (Located (Value io)) }
    , keywords : Dict String (Located (Value io))
    , symbols : Dict String (Located (Value io))
    , fns : List ( Value io, Located (Value io) )
    , maps : List ( Value io, Located (Value io) )
    , mapEntries : List ( Value io, Located (Value io) )
    , lists : List ( Value io, Located (Value io) )
    , refs : List ( Value io, Located (Value io) )
    , sets : List ( Value io, Located (Value io) )
    , throwables : List ( Value io, Located (Value io) )
    , vectors : List ( Value io, Located (Value io) )
    }


type ValueSet io
    = ValueSet
        { ints : Set.Set Int
        , floats : Set.Set Float
        , strings : Set.Set String
        , nil : Maybe (Value io)
        , bools : { true : Bool, false : Bool }
        , keywords : Set.Set String
        , symbols : Set.Set String
        , fns : List (Value io)
        , maps : List (ValueMap io)
        , mapEntries : List (ValueMapEntry io)
        , lists : List (List (Located (Value io)))
        , refs : List (Value io)
        , sets : List (ValueSet io)
        , throwables : List (Value io)
        , vectors : List (Array (Located (Value io)))
        }


type Number
    = Float Float
    | Int Int


type Value io
    = Number Number
    | String String
    | Ref String (Located (Value io))
    | Fn (Maybe String) ({ self : Value io, k : Continuation io } -> Thunk io)
    | List (List (Located (Value io)))
    | Vector (Array (Located (Value io)))
    | Nil
    | Bool Basics.Bool
    | Keyword String
    | Map (ValueMap io)
    | MapEntry (ValueMapEntry io)
    | Set (ValueSet io)
    | Symbol String
    | Throwable Exception


type alias StackFrame =
    { name : String
    , location : Located.Location
    }


type alias Env io =
    { global : Dict String (Value io)
    , local : Dict String (Value io)
    , stack : List StackFrame
    }
