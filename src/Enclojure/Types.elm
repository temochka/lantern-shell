module Enclojure.Types exposing (..)

import Dict exposing (Dict)
import Enclojure.Located exposing (Located)
import Set


fakeLoc : { start : ( number, number ), end : ( number, number ) }
fakeLoc =
    { start = ( 0, 0 ), end = ( 0, 0 ) }


type Exception
    = Exception String


type alias Continuation =
    Located Value -> Env -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )


type Thunk
    = Thunk Continuation


type alias Step =
    ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )


type Arity a
    = Fixed (a -> Env -> Continuation -> ( Result Exception ( IO, Env ), Maybe Thunk ))
    | Variadic ({ args : a, rest : List Value } -> Env -> Continuation -> ( Result Exception ( IO, Env ), Maybe Thunk ))


type IO
    = Const Value
    | Sleep Float


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
    , vectors : List ( Value, Located Value )
    }


type alias ValueSet =
    { ints : Set.Set Int
    , floats : Set.Set Float
    , strings : Set.Set String
    , nil : Maybe Value
    , bools : { true : Maybe Value, false : Maybe Value }
    , keywords : Set.Set String
    , symbols : Set.Set String
    , fns : List Value
    , maps : List Value
    , mapEntries : List Value
    , lists : List Value
    , refs : List Value
    , sets : List Value
    , vectors : List Value
    }


type Value
    = Int Int
    | Float Float
    | String String
    | Ref String (Located Value)
    | Fn (Maybe String) ({ self : Value, k : Continuation } -> Thunk)
    | List (List (Located Value))
    | Vector (List (Located Value))
    | Nil
    | Bool Basics.Bool
    | Keyword String
    | Map ValueMap
    | MapEntry ValueMapEntry
    | Set ValueSet
    | Symbol String


type alias Env =
    { global : Dict String Value
    , local : Dict String Value
    }
