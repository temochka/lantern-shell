module Enclojure.Lib exposing
    ( apply
    , apply_
    , assoc
    , conj
    , cons
    , dissoc
    , div
    , first
    , get
    , isEqual
    , isFloat
    , isGreaterThan
    , isGreaterThanOrEqual
    , isInteger
    , isLessThan
    , isLessThanOrEqual
    , isNotEqual
    , isNumber
    , list
    , minus
    , mul
    , newException
    , not_
    , peek
    , plus
    , prelude
    , rem
    , rest_
    , seq
    , sleep
    , str
    , throw
    )

import Array
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime exposing (emptyCallable, inspect)
import Enclojure.Types exposing (..)
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Html exposing (s)


pure : (a -> Result Exception IO) -> (a -> Env -> Continuation -> Runtime.Step)
pure fn =
    \v env k -> ( fn v |> Result.map (\io -> ( io, env )), Just (Thunk k) )


apply : Located Value -> Located Value -> Env -> Continuation -> Step
apply ((Located fnLoc fnExpr) as fn) arg env k =
    case fnExpr of
        Fn _ callable ->
            ( Ok ( Located.map Const arg, env ), Just (callable { self = fnExpr, k = k }) )

        _ ->
            ( Err (Located fnLoc (Exception (Runtime.inspectLocated fn ++ " is not a valid callable.")))
            , Just (Thunk k)
            )


sleep : Callable
sleep =
    let
        arity1 val =
            case val of
                Number (Int i) ->
                    Ok (Sleep (toFloat i))

                Number (Float i) ->
                    Ok (Sleep i)

                _ ->
                    Err (Exception "type error: sleep expects one integer argument")
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure arity1))
    }


not_ : Callable
not_ =
    let
        arity1 val =
            Ok (Const (Bool (not (Runtime.isTruthy val))))
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure arity1))
    }


list : Callable
list =
    let
        arity0 { rest } =
            Ok (Const (List (List.map (Located fakeLoc) rest)))
    in
    { emptyCallable | arity0 = Just (Variadic (pure arity0)) }


toNumbers : List Value -> Result Exception (List Number)
toNumbers =
    List.foldl
        (\e a -> a |> Result.andThen (\l -> toNumber e |> Result.map (\n -> n :: l)))
        (Ok [])
        >> Result.map List.reverse


flip : (a -> b -> c) -> (b -> a -> c)
flip f =
    \a b -> f b a


varargOp : { arity0 : Maybe Number, arity1 : Maybe (Number -> Number), arity2 : Number -> Number -> Number } -> Callable
varargOp { arity0, arity1, arity2 } =
    let
        wrappedArity2 { args, rest } =
            let
                ( argA, argB ) =
                    args
            in
            Result.map3
                (\a b ->
                    List.foldr (flip arity2) (arity2 a b) >> Number
                )
                (toNumber argA)
                (toNumber argB)
                (toNumbers rest)
    in
    { emptyCallable
        | arity0 = arity0 |> Maybe.map (Number >> Const >> Ok >> always >> pure >> Fixed)
        , arity1 =
            arity1
                |> Maybe.map
                    (\fn ->
                        Fixed <| pure <| (toNumber >> Result.map (fn >> Number >> Const))
                    )
        , arity2 = (wrappedArity2 >> Result.map Const) |> pure |> Variadic |> Just
    }


plus : Callable
plus =
    varargOp
        { arity0 = Just (Int 0)
        , arity1 = Just identity
        , arity2 =
            addNumbers
        }


addNumbers : Number -> Number -> Number
addNumbers =
    \numA numB ->
        case ( numA, numB ) of
            ( Int a, Int b ) ->
                Int (a + b)

            ( Int a, Float b ) ->
                Float (toFloat a + b)

            ( Float a, Int b ) ->
                Float (a + toFloat b)

            ( Float a, Float b ) ->
                Float (a + b)


negateNumber : Number -> Number
negateNumber numX =
    case numX of
        Int x ->
            Int (negate x)

        Float x ->
            Float (negate x)


minus : Callable
minus =
    varargOp
        { arity0 = Nothing
        , arity1 = Just negateNumber
        , arity2 =
            \numA numB ->
                case ( numA, numB ) of
                    ( Int a, Int b ) ->
                        Int (a - b)

                    ( Int a, Float b ) ->
                        Float (toFloat a - b)

                    ( Float a, Int b ) ->
                        Float (a - toFloat b)

                    ( Float a, Float b ) ->
                        Float (a - b)
        }


mul : Callable
mul =
    varargOp
        { arity0 = Just (Int 1)
        , arity1 = Just identity
        , arity2 =
            \numA numB ->
                case ( numA, numB ) of
                    ( Int a, Int b ) ->
                        Int (a * b)

                    ( Int a, Float b ) ->
                        Float (toFloat a * b)

                    ( Float a, Int b ) ->
                        Float (a * toFloat b)

                    ( Float a, Float b ) ->
                        Float (a * b)
        }


div : Callable
div =
    let
        op numA numB =
            case ( numA, numB ) of
                ( Int a, Int b ) ->
                    Int (a // b)

                ( Int a, Float b ) ->
                    Float (toFloat a / b)

                ( Float a, Int b ) ->
                    Float (a / toFloat b)

                ( Float a, Float b ) ->
                    Float (a / b)
    in
    varargOp
        { arity0 = Nothing
        , arity1 = Just (op (Int 1))
        , arity2 = op
        }


remainderByFloat : Float -> Float -> Float
remainderByFloat by x =
    x - (toFloat (floor (x / by)) * by)


rem : Callable
rem =
    let
        op numA numB =
            case ( numA, numB ) of
                ( Int a, Int b ) ->
                    Int (remainderBy b a)

                ( Int a, Float b ) ->
                    Float (remainderByFloat b (toFloat a))

                ( Float a, Int b ) ->
                    Float (remainderByFloat (toFloat b) a)

                ( Float a, Float b ) ->
                    Float (remainderByFloat b a)

        arity2 ( valA, valB ) =
            Result.map2 op
                (toNumber valA)
                (toNumber valB)
    in
    { emptyCallable
        | arity2 = Just <| Fixed <| pure <| (arity2 >> Result.map (Number >> Const))
    }


toNumber : Value -> Result Exception Number
toNumber val =
    case val of
        Number n ->
            Ok n

        _ ->
            Err <| Exception (inspect val ++ " is not a number")


isLessThan : Callable
isLessThan =
    compOp { intOp = (<), floatOp = (<), stringOp = (<) }


isLessThanOrEqual : Callable
isLessThanOrEqual =
    compOp { intOp = (<=), floatOp = (<=), stringOp = (<=) }


isGreaterThan : Callable
isGreaterThan =
    compOp { intOp = (>), floatOp = (>), stringOp = (<=) }


isGreaterThanOrEqual : Callable
isGreaterThanOrEqual =
    compOp { intOp = (>=), floatOp = (>=), stringOp = (>=) }


compOp :
    { intOp : Int -> Int -> Bool
    , floatOp : Float -> Float -> Bool
    , stringOp : String -> String -> Bool
    }
    -> Callable
compOp { intOp, floatOp, stringOp } =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                result =
                    case args of
                        ( Number (Float a), Number (Float b) ) ->
                            Ok (floatOp a b)

                        ( Number (Float a), Number (Int b) ) ->
                            Ok (floatOp a (toFloat b))

                        ( Number (Int a), Number (Float b) ) ->
                            Ok (floatOp (toFloat a) b)

                        ( Number (Int a), Number (Int b) ) ->
                            Ok (intOp a b)

                        ( String a, String b ) ->
                            Ok (stringOp a b)

                        ( a, b ) ->
                            Err (Exception ("can't compare " ++ inspect a ++ " and " ++ inspect b))
            in
            case rest of
                [] ->
                    result |> Result.map Bool

                nextVal1 :: nextRest ->
                    result
                        |> Result.andThen
                            (\r ->
                                if r then
                                    arity2 { args = ( Tuple.second args, nextVal1 ), rest = nextRest }

                                else
                                    Ok (Bool False)
                            )
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }


isEqual : Callable
isEqual =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (a == b))

                nextVal1 :: nextRest ->
                    if a == b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }


isNotEqual : Callable
isNotEqual =
    let
        arity1 _ =
            Ok (Bool True)

        arity2 { args, rest } =
            let
                ( a, b ) =
                    args
            in
            case rest of
                [] ->
                    Ok (Bool (a /= b))

                nextVal1 :: nextRest ->
                    if a /= b then
                        arity2 { args = ( b, nextVal1 ), rest = nextRest }

                    else
                        Ok (Bool False)
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
        , arity2 = Just (Variadic (pure (arity2 >> Result.map Const)))
    }


str : Callable
str =
    let
        arity0 { rest } =
            rest
                |> List.map Runtime.toString
                |> String.join ""
                |> String
                |> Ok
    in
    { emptyCallable
        | arity0 = Just (Variadic (pure (arity0 >> Result.map Const)))
    }


seq : Callable
seq =
    let
        arity1 coll =
            case coll of
                List [] ->
                    Ok Nil

                List l ->
                    Ok (List l)

                Vector l ->
                    if Array.isEmpty l then
                        Ok Nil

                    else
                        Ok (List (Array.toList l))

                Set s ->
                    if ValueSet.isEmpty s then
                        Ok Nil

                    else
                        Ok (List (ValueSet.toList s |> List.map Located.fakeLoc))

                Map m ->
                    if ValueMap.isEmpty m then
                        Ok Nil

                    else
                        Ok (List (ValueMap.toList m |> List.map (MapEntry >> Located.fakeLoc)))

                Nil ->
                    Ok (List [])

                _ ->
                    Err (Exception (inspect coll ++ " is not sequable"))
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
    }


fixedCall : Maybe (Arity a) -> a -> Env -> Continuation -> ( Result Exception ( IO, Env ), Maybe Thunk )
fixedCall mArity =
    mArity
        |> Maybe.andThen
            (\arity ->
                case arity of
                    Fixed a ->
                        Just a

                    Variadic _ ->
                        Nothing
            )
        |> Maybe.withDefault
            (\_ _ k ->
                ( Err (Exception "Interpreter error: undefined internal call arity")
                , Just (Thunk k)
                )
            )


cons : Callable
cons =
    let
        arity2 ( x, coll ) env1 k =
            fixedCall
                seq.arity1
                coll
                env1
                (\(Located collLoc collSeq) env2 ->
                    case collSeq of
                        List l ->
                            ( Ok ( Located collLoc (Const (List (Located.fakeLoc x :: l))), env2 ), Just (Thunk k) )

                        Nil ->
                            ( Ok ( Located.fakeLoc (Const (List [ Located.fakeLoc x ])), env2 )
                            , Just (Thunk k)
                            )

                        _ ->
                            ( Err (Located.fakeLoc (Exception "Interpreter error: seq returned a non-list"))
                            , Just (Thunk k)
                            )
                )
    in
    { emptyCallable
        | arity2 = Just (Fixed arity2)
    }


conj : Callable
conj =
    let
        arity2 signature =
            let
                ( coll, x ) =
                    signature.args

                xs =
                    x :: signature.rest |> List.map Located.fakeLoc
            in
            case coll of
                List l ->
                    xs
                        |> List.foldl (::) l
                        |> List
                        |> Ok

                Vector a ->
                    xs
                        |> List.foldr Array.push a
                        |> Vector
                        |> Ok

                Nil ->
                    Ok <| List (List.reverse xs)

                Set s ->
                    xs
                        |> List.map Located.getValue
                        |> List.foldl ValueSet.insert s
                        |> Set
                        |> Ok

                _ ->
                    Err (Exception ("don't know how to conj to " ++ inspect coll))
    in
    { emptyCallable
        | arity2 = Just <| Variadic <| pure (arity2 >> Result.map Const)
    }


first : Callable
first =
    let
        arity1 collVal =
            collVal
                |> Runtime.toSeq
                |> Result.map
                    (\s ->
                        case s of
                            x :: _ ->
                                x

                            [] ->
                                Nil
                    )
    in
    { emptyCallable
        | arity1 = Just (Fixed <| pure (arity1 >> Result.map Const))
    }


peek : Callable
peek =
    let
        arity1 val =
            case val of
                List l ->
                    Ok <| (List.head l |> Maybe.map Located.getValue |> Maybe.withDefault Nil)

                Vector v ->
                    Ok <| (Array.get (Array.length v - 1) v |> Maybe.map Located.getValue |> Maybe.withDefault Nil)

                Nil ->
                    Ok Nil

                _ ->
                    Err <| Exception ("Cannot use " ++ inspect val ++ " as a queue")
    in
    { emptyCallable
        | arity1 = Just (Fixed (pure (arity1 >> Result.map Const)))
    }


isNumber : Callable
isNumber =
    let
        arity1 v =
            case v of
                Number _ ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
    }


isInteger : Callable
isInteger =
    let
        arity1 v =
            case v of
                Number (Int _) ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
    }


isFloat : Callable
isFloat =
    let
        arity1 v =
            case v of
                Number (Float _) ->
                    Bool True

                _ ->
                    Bool False
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Const >> Ok)
    }


rest_ : Callable
rest_ =
    let
        arity1 coll env1 k =
            fixedCall
                seq.arity1
                coll
                env1
                (\(Located collLoc collSeq) env2 ->
                    case collSeq of
                        List (_ :: rst) ->
                            ( Ok ( Located collLoc (Const (List rst)), env2 ), Just (Thunk k) )

                        List _ ->
                            ( Ok ( Located collLoc (Const Nil), env2 ), Just (Thunk k) )

                        Nil ->
                            ( Ok ( Located.fakeLoc (Const (List [])), env2 )
                            , Just (Thunk k)
                            )

                        _ ->
                            ( Err (Located.fakeLoc (Exception "Interpreter error: seq returned a non-list"))
                            , Just (Thunk k)
                            )
                )
    in
    { emptyCallable
        | arity1 = Just (Fixed arity1)
    }


throw : Callable
throw =
    let
        arity1 v =
            case v of
                Throwable e ->
                    Err e

                _ ->
                    Err (Exception (inspect v ++ " is not throwable"))
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure arity1
    }


newException : Callable
newException =
    let
        arity1 v =
            case v of
                String s ->
                    Ok (Throwable (Exception s))

                _ ->
                    Err (Exception "exception message must be a string")
    in
    { emptyCallable
        | arity1 = Just <| Fixed <| pure (arity1 >> Result.map Const)
    }



-- ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )


toRuntimeStep : Step -> Runtime.Step
toRuntimeStep ( r, k ) =
    ( r |> Result.mapError Located.getValue |> Result.map (Tuple.mapFirst Located.getValue), k )


apply_ : Callable
apply_ =
    let
        arity2 signature env k =
            let
                ( fn, firstArg ) =
                    signature.args

                args =
                    firstArg :: signature.rest

                numArgs =
                    List.length args

                posArgs =
                    List.take (numArgs - 1) args

                listArgsResult =
                    args
                        |> List.drop (numArgs - 1)
                        |> List.head
                        |> Result.fromMaybe (Exception "Interpreter error: arity2 function doesn't have a 2nd argument")
                        |> Result.andThen Runtime.toSeq
            in
            case listArgsResult of
                Ok listArgs ->
                    apply
                        (Located.fakeLoc fn)
                        (Located.fakeLoc (List (List.map Located.fakeLoc (posArgs ++ listArgs))))
                        env
                        k
                        |> toRuntimeStep

                Err e ->
                    ( Err e, Just (Thunk k) )
    in
    { emptyCallable
        | arity2 = Just <| Variadic arity2
    }


get : Callable
get =
    let
        arity2 ( mapVal, key ) =
            arity3 ( mapVal, key, Nil )

        arity3 ( mapVal, key, default ) =
            (case mapVal of
                Map m ->
                    ValueMap.get key m |> Maybe.map Located.getValue

                Vector l ->
                    case key of
                        Number (Int i) ->
                            l |> Array.get i |> Maybe.map Located.getValue

                        _ ->
                            Nothing

                Set s ->
                    if ValueSet.member key s then
                        Just key

                    else
                        Nothing

                _ ->
                    Just Nil
            )
                |> Maybe.withDefault default
    in
    { emptyCallable
        | arity2 = Just <| Fixed <| pure (arity2 >> Const >> Ok)
        , arity3 = Just <| Fixed <| pure (arity3 >> Const >> Ok)
    }


listToPairs : List a -> Maybe (List ( a, a ))
listToPairs l =
    case l of
        [] ->
            Just []

        a :: b :: rest ->
            listToPairs rest |> Maybe.map ((::) ( a, b ))

        _ ->
            Nothing


assoc : Callable
assoc =
    let
        arity3 signature =
            let
                ( val, firstK, firstV ) =
                    signature.args

                kvs =
                    listToPairs signature.rest
                        |> Maybe.map ((::) ( firstK, firstV ))
                        |> Result.fromMaybe (Exception "invalid number of key/value args to assoc")

                keysToInt l =
                    case l of
                        [] ->
                            Ok []

                        ( k, v ) :: rest ->
                            case k of
                                Number (Int i) ->
                                    keysToInt rest |> Result.map ((::) ( i, v ))

                                _ ->
                                    Err (Exception "Key must be integer")
            in
            case val of
                Map m ->
                    kvs
                        |> Result.map (List.foldr (\( k, v ) a -> ValueMap.insert k (Located.fakeLoc v) a) m)
                        |> Result.map Map

                Vector array ->
                    kvs
                        |> Result.andThen keysToInt
                        |> Result.map (List.foldr (\( k, v ) a -> Array.set k (Located.fakeLoc v) a) array)
                        |> Result.map Vector

                Nil ->
                    kvs
                        |> Result.map (List.foldr (\( k, v ) a -> ValueMap.insert k (Located.fakeLoc v) a) ValueMap.empty)
                        |> Result.map Map

                _ ->
                    Err (Exception (inspect val ++ " is not associable"))
    in
    { emptyCallable | arity3 = Just <| Variadic <| pure (arity3 >> Result.map Const) }


dissoc : Callable
dissoc =
    let
        arity2 signature =
            let
                ( val, firstKey ) =
                    signature.args

                keys =
                    firstKey :: signature.rest
            in
            case val of
                Map m ->
                    keys
                        |> List.foldr (\k a -> ValueMap.remove k a) m
                        |> Map
                        |> Ok

                _ ->
                    Err (Exception (inspect val ++ " is not dissociable"))
    in
    { emptyCallable | arity2 = Just <| Variadic <| pure (arity2 >> Result.map Const) }


prelude : String
prelude =
    """
(defn complement [f]
  (fn [& args] (not (apply f args))))

(defn identity [a] a)

(defn comp [& fns]
  (reduce
    (fn [a e] (fn [& args] (a (apply e args))))
    identity
    fns))

(defn last
  [coll]
  (if (next coll)
    (last (rest coll))
    (first coll)))

(defn next [coll]
  (seq (rest coll)))

(defn reverse [coll]
  (reduce (fn [a e] (cons e a)) (list) coll))

(defn concat [& colls]
  (when-let [coll (first colls)]
    (reduce
     (fn [a e] (cons e a))
     (apply concat (rest colls))
     (reverse coll))))

(defn map [f coll]
  (if (seq coll)
    (cons (f (first coll)) (map f (rest coll)))
    (list)))

(defn mapcat [f coll]
  (if (seq coll)
    (concat (f (first coll)) (mapcat f (rest coll)))
    (list)))

(defn filter [pred coll]
  (if (seq coll)
    (let [el (first coll)]
      (if (pred el)
        (cons el (filter pred (rest coll)))
        (filter pred (rest coll))))
    (list)))

(defn remove [pred coll]
  (filter (complement pred) coll))

(defn reduce
  ([f coll]
   (reduce f (f) coll))
  ([f init coll]
   (if (seq coll)
     (reduce f (f init (first coll)) (rest coll))
     init)))

(defn pos? [x]
  (< 0 x))

(defn neg? [x]
  (< x 0))

(defn zero? [x]
  (= x 0))

(defn inc [x]
  (+ x 1))

(defn dec [x]
  (- x 1))

(defn mod
  [num div]
  (let [m (rem num div)]
    (if (or (zero? m) (= (pos? num) (pos? div)))
      m
      (+ m div))))

(defn even?
  [n]
  (if (integer? n)
    (zero? (rem n 2))
    (throw (Exception. (str "Argument must be an integer: " n)))))

(defn odd? [n]
  (not (even? n)))

(defn get-in
  ([m ks]
   (reduce get m ks))
  ([m ks not-found]
   (reduce #(get %1 %2 not-found) m ks)))

(defn assoc-in
  ([m ks v]
   (let [k (first ks)
         ks (rest ks)]
     (if (seq ks)
       (assoc m k (assoc-in (get m k) ks v))
       (assoc m k v)))))

(defn update
  ([m k f & args]
   (assoc m k (apply f (get m k) args))))

(defn update-in
  ([m ks f & args]
   (assoc-in m ks (apply f (get-in m ks) args))))

"""
