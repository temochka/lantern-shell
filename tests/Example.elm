module Example exposing (..)

import Array
import Enclojure
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Env, Exception(..), Number(..), Value(..))
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Http exposing (Expect)
import Test exposing (..)


type Expectation
    = ExpectValue Value
    | ExpectException String
    | ExpectExceptionWithTrace String (List String)


eval : String -> Result Exception Value
eval code =
    Enclojure.evalPure Enclojure.init code
        |> Result.mapError Located.getValue
        |> Result.map Tuple.first


expectValue : Value -> String -> Test
expectValue value code =
    (\_ -> eval code |> Expect.equal (Ok value)) |> test code


expectException : String -> String -> Test
expectException expectedMsg code =
    (\_ ->
        case eval code of
            Err (Exception actualMsg _) ->
                Expect.equal expectedMsg actualMsg

            Ok _ ->
                Expect.fail "no exception thrown"
    )
        |> test code


expectExceptionWithTrace : String -> List String -> String -> Test
expectExceptionWithTrace expectedMsg expectedTrace code =
    (\_ ->
        case eval code of
            Ok _ ->
                Expect.fail "No exception"

            Err ((Exception actualMsg _) as actualException) ->
                Expect.all
                    [ always <| Expect.equal expectedMsg actualMsg
                    , always <| Expect.equal expectedTrace (Runtime.prettyTrace actualException)
                    ]
                    ()
    )
        |> test code


suite : Test
suite =
    describe "Enclojure"
        [ describe "booleans"
            [ "true" |> expectValue (Bool True)
            , "false" |> expectValue (Bool False)
            ]
        , describe "nils"
            [ "nil" |> expectValue Nil ]
        , describe "numbers"
            [ "42" |> expectValue (Number <| Int 42)
            , "42.0" |> expectValue (Number <| Float 42.0)
            , "-42" |> expectValue (Number <| Int -42)
            , "-42.0" |> expectValue (Number <| Float -42.0)
            ]
        , describe "strings"
            [ "\"\"" |> (expectValue <| String "")
            , "\"" |> (expectException <| "Missing closing quote for a string at row 1, col 2")
            , "\"foo\"" |> (expectValue <| String "foo")
            , "\"\\n\"" |> (expectValue <| String "\n")
            , "\"\\t\"" |> (expectValue <| String "\t")
            , "\"\\\\\"" |> (expectValue <| String "\\")
            , "\"\\\"\"" |> (expectValue <| String "\"")
            ]
        , describe "keywords"
            [ ":foo" |> (expectValue <| Keyword "foo")
            , ":a-b$c_d?e+f/g*h>i<j=k'l&m%n?o.p$q_" |> (expectValue <| Keyword "a-b$c_d?e+f/g*h>i<j=k'l&m%n?o.p$q_")
            , "(:foo {:foo 42})" |> (expectValue <| Number <| Int 42)
            , "(:foo nil :bar)" |> (expectValue <| Keyword "bar")
            , "(:foo {})" |> expectValue Nil
            , "(:foo #{})" |> expectValue Nil
            , "(:foo #{:foo})" |> (expectValue <| Keyword "foo")
            ]
        , describe "symbols"
            [ "(quote symbol)" |> (expectValue <| Symbol "symbol")
            , "(quote nil-symbol)" |> (expectValue <| Symbol "nil-symbol")
            , "(quote false-symbol)" |> (expectValue <| Symbol "false-symbol")
            , "(quote true-symbol)" |> (expectValue <| Symbol "true-symbol")
            , "(quote symbol-nil)" |> (expectValue <| Symbol "symbol-nil")
            , "(quote symbol-false)" |> (expectValue <| Symbol "symbol-false")
            , "(quote symbol-true)" |> (expectValue <| Symbol "symbol-true")
            ]
        , describe "lists"
            [ "()" |> (expectValue <| List [])
            ]
        , describe "vectors"
            [ "[]" |> (expectValue <| Vector Array.empty) ]
        , describe "maps"
            [ "{}" |> (expectValue <| Map ValueMap.empty)
            , "({:a 1} :a)" |> (expectValue <| Number <| Int 1)
            , "({\"a\" 2} \"a\")" |> (expectValue <| Number <| Int 2)
            , "({[] 3} [])" |> (expectValue <| Number <| Int 3)
            , "({nil 4} nil)" |> (expectValue <| Number <| Int 4)
            , "({true 5} true)" |> (expectValue <| Number <| Int 5)
            , "({false 6} false)" |> (expectValue <| Number <| Int 6)
            , "({7 7} 7)" |> (expectValue <| Number <| Int 7)
            , "({8.0 8} 8.0)" |> (expectValue <| Number <| Int 8)
            , "({() 9} ())" |> (expectValue <| Number <| Int 9)
            , "({{} 10} {})" |> (expectValue <| Number <| Int 10)
            ]
        , describe "sets"
            [ "#{}" |> (expectValue <| Set ValueSet.empty)
            , "(= #{42 42 42} #{42})" |> (expectValue <| Bool True)
            , "(#{} 42)" |> expectValue Nil
            , "(#{false} false)" |> (expectValue <| Bool False)
            , "(#{true} true)" |> (expectValue <| Bool True)
            , "(#{nil} nil)" |> expectValue Nil
            , "(#{42} 42)" |> (expectValue <| Number <| Int 42)
            , "(#{42.0} 42.0)" |> (expectValue <| Number <| Float 42.0)
            , "(#{\"a\"} \"a\")" |> (expectValue <| String "a")
            , "(#{:a} :a)" |> (expectValue <| Keyword "a")
            , "(#{[]} [])" |> (expectValue <| Vector Array.empty)
            , "(#{{}} {})" |> (expectValue <| Map ValueMap.empty)
            , "(#{()} ())" |> (expectValue <| List [])
            , "(#{#{}} #{})" |> (expectValue <| Set ValueSet.empty)
            ]

        -- special forms
        , describe "def"
            [ "(def foo 42) foo" |> (expectValue <| Number <| Int 42)
            , "(def foo 42) (def foo 43) foo" |> (expectValue <| Number <| Int 43)
            ]
        , describe "do"
            [ "(do)" |> expectValue Nil
            , "(do :foo)" |> (expectValue <| Keyword "foo")
            , """
                 (do
                  (def a 1)
                  (def b 2)
                  (def c 3)
                  #{a b c})
                 """
                |> (expectValue <| Set (ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Number <| Int 3 ]))
            ]
        , describe "if"
            [ "(if true 42)" |> (expectValue <| Number <| Int 42)
            , "(if false 42)" |> expectValue Nil
            , "(if true 42 43)" |> (expectValue <| Number <| Int 42)
            , "(if false 42 43)" |> (expectValue <| Number <| Int 43)
            , "(if true)" |> (expectException <| "an if without then")
            , "(if true (def a 1) (def b 2)) b" |> (expectException <| "Unknown symbol b")
            , "(if true (def a 1) (def b 2)) a" |> (expectValue <| Number <| Int 1)
            ]
        , describe "quote"
            [ "(quote foo)" |> (expectValue <| Symbol "foo")
            , "(= (quote (1 2 3)) (list 1 2 3))" |> (expectValue <| Bool True)
            ]
        , describe "let"
            [ "(let [a 1 b 2 c 3] (+ a b c))" |> (expectValue <| Number <| Int 6)
            , "(let [_ (def a 1) _ (def a 2)] a)" |> (expectValue <| Number <| Int 2)
            , "(let [[a b c] [2 3 4]] (+ a b c))" |> (expectValue <| Number <| Int 9)
            , "(let [[a & rst] [3 4 5]] (+ a (first rst) (second rst)))" |> (expectValue <| Number <| Int 12)
            , "(let [[a & [b c]] [4 5 6]] (+ a b c))" |> (expectValue <| Number <| Int 15)
            , "(let [{a :a} {:a 1}] a)" |> (expectValue <| Number <| Int 1)
            , "(let [{a :a} {}] a)" |> expectValue Nil
            , "(let [{a 1} [2 3 4]] a)" |> (expectValue <| Number <| Int 3)
            , "(let [{a 1} []] a)" |> expectValue Nil
            , "(let [{a :a :or {a 2}} {}] a)" |> (expectException <| ":or is not supported")
            , "(let [[:as foo] nil] foo)" |> expectValue Nil
            , "(let [[:as foo] []] foo)" |> (expectValue <| Vector <| Array.empty)
            , """
                 (let [[a b & rst :as all] [1 2 3 4 5]]
                  (= [a b rst all]
                     [1 2 (list 3 4 5) [1 2 3 4 5]]))
                 """
                |> (expectValue <| Bool True)
            , "(let [{:keys [a b c]} {:a 1 :b 2}] #{a b c})"
                |> (expectValue <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Nil ])
            , "(let [{:keys [a]} {\"a\" 3}] a)" |> expectValue Nil
            , "(let [{:keys [a]} nil] a)" |> expectValue Nil
            , "(let [{:keys [a]} []] a)" |> expectValue Nil
            , "(let [{:strs [a b c]} {:a 1 :b 2}] #{a b c})"
                |> (expectValue <| Set <| ValueSet.fromList [ Nil, Nil, Nil ])
            , "(let [{:strs [a b c]} {\"a\" 1 \"b\" 2}] #{a b c})"
                |> (expectValue <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Nil ])
            , "(let [{:strs [a]} nil] a)" |> expectValue Nil
            , "(let [{:strs [a]} []] a)" |> expectValue Nil
            , "(let [{:strs [a]} {}] a)" |> expectValue Nil
            ]
        , describe "fn"
            [ "((fn [] :a))" |> (expectValue <| Keyword "a")
            , "((fn [] :a :b :c))" |> (expectValue <| Keyword "c")
            , "((fn [a] a) :b)" |> (expectValue <| Keyword "b")
            , "((fn [a b] #{a b}) :a :b)" |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b" ])
            , "((fn [a b c] #{a b c}) :a :b :c)"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c" ])
            , "((fn [a b c d] #{a b c d}) :a :b :c :d)"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d" ])
            , "((fn [a b c d e] #{a b c d e}) :a :b :c :d :e)"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d", Keyword "e" ])
            , "((fn [& args] (into #{} args)) :a :b :c :d :e)"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d", Keyword "e" ])
            , "((fn [a & rst] (into #{} (cons a rst))) :a :b :c :d)"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d" ])
            , "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))))" |> (expectValue <| Number <| Int 42)
            , "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))) 43)" |> (expectValue <| Number <| Int 43)
            , "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))) 44 45 46)"
                |> (expectValue <| Set <| ValueSet.fromList [ Number <| Int 45, Number <| Int 46 ])
            , "((fn [n] (if (pos? n) (+ n (recur (dec n))) 0)) 10)" |> (expectValue <| Number <| Int 55)
            , "((fn foo [n] (if (pos? n) (+ n (foo (dec n))) 0)) 10)" |> (expectValue <| Number <| Int 55)
            ]
        , describe "#()"
            [ "(#(do :result))" |> (expectValue <| Keyword "result")
            , "(#(inc %) 1)" |> (expectValue <| Number <| Int 2)
            , "(#(inc %1) 1)" |> (expectValue <| Number <| Int 2)
            , "(#(inc %2) 1)" |> (expectException <| "Argument error: Too few arguments")
            , "(#(+ % %2) 1 2)" |> (expectValue <| Number <| Int 3)
            , "(#(+ %1 %2) 1 2)" |> (expectValue <| Number <| Int 3)
            , "(#(+ %1 (last %&)) 2 3 4)" |> (expectValue <| Number <| Int 6)
            , "(#(into #{} %&) 1 2 3)"
                |> (expectValue <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Number <| Int 3 ])
            , "#(map #(inc %) %)"
                |> expectException "Parsing error: nested #() are not supported, use fn instead. at row 1, col 18"
            ]
        , describe "#_"
            [ "#_() nil" |> (expectValue <| Nil)
            , "#_ () nil" |> (expectValue <| Nil)
            , "#_ (42 43 44) nil" |> (expectValue <| Nil)
            , "(+ 3 #_4 5)" |> (expectValue <| Number <| Int 8)
            ]

        -- operators
        , describe "+"
            [ "(+)" |> (expectValue <| Number <| Int 0)
            , "(+ 1)" |> (expectValue <| Number <| Int 1)
            , "(+ 1 2)" |> (expectValue <| Number <| Int 3)
            , "(+ 1 2 3 4 5)" |> (expectValue <| Number <| Int 15)
            , "(+ 1 0.2)" |> (expectValue <| Number <| Float 1.2)
            , "(+ 0.8 0.2)" |> (expectValue <| Number <| Float 1.0)
            ]
        , describe "-"
            [ "(-)" |> expectException "Invalid arity 0"
            , "(- 1)" |> (expectValue <| Number <| Int -1)
            , "(- 1 2)" |> (expectValue <| Number <| Int -1)
            , "(- 1 2 3 4 5)" |> (expectValue <| Number <| Int -13)
            ]
        , describe "*"
            [ "(*)" |> (expectValue <| Number <| Int 1)
            , "(* 2)" |> (expectValue <| Number <| Int 2)
            , "(* 2 3)" |> (expectValue <| Number <| Int 6)
            , "(* 1 2 3 4 5)" |> (expectValue <| Number <| Int 120)
            , "(* 2 1.5)" |> (expectValue <| Number <| Float 3.0)
            ]
        , describe "/"
            [ "(/)" |> expectException "Invalid arity 0"
            , "(/ 2)" |> (expectValue <| Number <| Int 0)
            , "(/ 4 2)" |> (expectValue <| Number <| Int 2)
            , "(/ 16 2 2)" |> (expectValue <| Number <| Int 4)
            , "(/ 5 2)" |> (expectValue <| Number <| Int 2)
            , "(/ 5.0 2)" |> (expectValue <| Number <| Float 2.5)
            , "(/ 5 2.0)" |> (expectValue <| Number <| Float 2.5)
            ]
        , describe "="
            [ "(=)" |> expectException "Invalid arity 0"
            , "(= nil)" |> (expectValue <| Bool True)
            , "(= 1 1)" |> (expectValue <| Bool True)
            , "(= 1 2)" |> (expectValue <| Bool False)
            ]
        , describe "not="
            [ "(not=)" |> expectException "Invalid arity 0"
            , "(not= nil)" |> (expectValue <| Bool False)
            , "(not= 1 1)" |> (expectValue <| Bool False)
            , "(not= 1 2)" |> (expectValue <| Bool True)
            ]
        , describe ">"
            [ "(>)" |> expectException "Invalid arity 0"
            , "(> 1)" |> (expectValue <| Bool True)
            , "(> 1 1)" |> (expectValue <| Bool False)
            , "(> 2 1)" |> (expectValue <| Bool True)
            , "(> 1 2)" |> (expectValue <| Bool False)
            , "(> \"a\" \"b\")" |> (expectValue <| Bool False)
            , "(> 4.0 2)" |> (expectValue <| Bool True)
            , "(> 2.0 4.0)" |> (expectValue <| Bool False)
            ]
        , describe ">="
            [ "(>=)" |> expectException "Invalid arity 0"
            , "(>= 1)" |> (expectValue <| Bool True)
            , "(>= 1 1)" |> (expectValue <| Bool True)
            , "(>= 2 1)" |> (expectValue <| Bool True)
            , "(>= 1 2)" |> (expectValue <| Bool False)
            , "(>= \"a\" \"b\")" |> (expectValue <| Bool False)
            , "(>= \"a\" \"a\")" |> (expectValue <| Bool True)
            , "(>= 4.0 2)" |> (expectValue <| Bool True)
            , "(>= 2.0 4.0)" |> (expectValue <| Bool False)
            ]
        , describe "<"
            [ "(<)" |> expectException "Invalid arity 0"
            , "(< 1)" |> (expectValue <| Bool True)
            , "(< 1 1)" |> (expectValue <| Bool False)
            , "(< 2 1)" |> (expectValue <| Bool False)
            , "(< 1 2)" |> (expectValue <| Bool True)
            , "(< \"a\" \"b\")" |> (expectValue <| Bool True)
            , "(< 4.0 2)" |> (expectValue <| Bool False)
            , "(< 2.0 4.0)" |> (expectValue <| Bool True)
            ]
        , describe "<="
            [ "(<=)" |> expectException "Invalid arity 0"
            , "(<= 1)" |> (expectValue <| Bool True)
            , "(<= 1 1)" |> (expectValue <| Bool True)
            , "(<= 2 1)" |> (expectValue <| Bool False)
            , "(<= 1 2)" |> (expectValue <| Bool True)
            , "(<= \"a\" \"b\")" |> (expectValue <| Bool True)
            , "(<= \"a\" \"a\")" |> (expectValue <| Bool True)
            , "(<= 4.0 2)" |> (expectValue <| Bool False)
            , "(<= 2.0 4.0)" |> (expectValue <| Bool True)
            ]
        , describe "'"
            [ "'(foo)" |> (expectValue <| List [ Located.at ( 1, 3 ) ( 1, 6 ) (Symbol "foo") ])
            , "'symbol" |> (expectValue <| Symbol "symbol")
            , "' symbol" |> (expectValue <| Symbol "symbol")
            , "'()" |> (expectValue <| List [])
            ]

        -- built-in macros
        , describe "and"
            [ "(and :foo :bar)" |> (expectValue <| Keyword "bar")
            , "(and (def a :foo) (def b :bar) #{a b})"
                |> (expectValue <| Set <| ValueSet.fromList [ Keyword "foo", Keyword "bar" ])
            , "(and nil :bar)" |> expectValue Nil
            , "(and true true true true true false :foo)" |> (expectValue <| Bool False)
            ]
        , describe "cond"
            [ "(cond (< 2 4) :foo)" |> (expectValue <| Keyword "foo")
            , "(cond (< 4 2) :foo (pos? 0) :bar (zero? 0) :buz)" |> (expectValue <| Keyword "buz")
            , "(cond (< 4 2) :foo :let [bar 42] (pos? bar) bar)" |> (expectValue <| Number <| Int 42)
            , "(cond (< 4 2) :foo :else :else)" |> (expectValue <| Keyword "else")
            ]
        , describe "defn"
            [ """
                 (defn foo
                   [n]
                   n)

                 (foo :ret)
                 """
                |> (expectValue <| Keyword "ret")
            , """
                 (defn weird-plus
                   ([n]
                    (weird-plus n 2))
                   ([a b]
                    (+ a b)))

                 (+ (weird-plus 4) (weird-plus 6 1))
                 """
                |> (expectValue <| Number <| Int 13)
            ]
        , describe "if-let"
            [ "(if-let [a nil] a :else)" |> (expectValue <| Keyword "else")
            , "(if-let [a :then] a :else)" |> (expectValue <| Keyword "then")
            , "(if-let [a :then] a :else :or-else)" |> (expectException <| "an if with too many forms")
            ]
        , describe "or"
            [ "(or)" |> expectValue Nil
            , "(or false)" |> expectValue Nil
            , "(or false false false false true)" |> (expectValue <| Bool True)
            , "(or true (def a 42)) a" |> (expectException <| "Unknown symbol a")
            ]
        , describe "some->"
            [ "(some-> :ret)" |> (expectValue <| Keyword "ret")
            , "(some-> false)" |> (expectValue <| Bool False)
            , "(some-> nil inc)" |> expectValue Nil
            , "(some-> 0 inc inc)" |> (expectValue <| Number <| Int 2)
            , "(some-> [] seq inc)" |> expectValue Nil
            ]
        , describe "some->>"
            [ "(some->> :ret)" |> (expectValue <| Keyword "ret")
            , "(some->> false)" |> (expectValue <| Bool False)
            , "(some->> [1] (map inc) (reduce +)))" |> (expectValue <| Number <| Int 2)
            , "(some->> [1] (map inc) (drop 1))" |> (expectValue <| List [])
            , "(some->> [1] (map inc) (drop 1) seq (cons 1))" |> expectValue Nil
            , "(some->> [1] (map inc) (drop 1) (cons 1) (into #{}))"
                |> (expectValue <| Set <| ValueSet.fromList [ Number <| Int 1 ])
            ]
        , describe "when"
            [ "(when true 1 2 3 4)" |> (expectValue <| Number <| Int 4)
            , "(when true (def a 1) (def b 2) (def c 3) (+ a b c))" |> (expectValue <| Number <| Int 6)
            , "(when false 42)" |> expectValue Nil
            ]
        , describe "when-let"
            [ "(when-let [a nil] 42 43)" |> expectValue Nil
            , "(when-let [a :ret] 42 a)" |> (expectValue <| Keyword "ret")
            , "(when-let [a 3] (def b 2) (+ a b))" |> (expectValue <| Number <| Int 5)
            ]
        , describe "when-not"
            [ "(when-not false 1 2 3 4)" |> (expectValue <| Number <| Int 4)
            , "(when-not false (def a 1) (def b 2) (def c 3) (+ a b c))" |> (expectValue <| Number <| Int 6)
            , "(when-not true 42)" |> expectValue Nil
            ]
        , describe "->"
            [ "(-> :ret)" |> (expectValue <| Keyword "ret")
            , "(-> {:foo :bar} (assoc :buz :boo) (dissoc :foo :buz))" |> (expectValue <| Map <| ValueMap.empty)
            , "(-> [] seq)" |> expectValue Nil
            , "(-> {} (-> (assoc :bar :buz)) (dissoc :bar) seq)" |> expectValue Nil
            ]
        , describe "->>"
            [ "(->> :ret)" |> (expectValue <| Keyword "ret")
            , "(->> [1 2 3] (map inc) (map inc) (map dec) (= (list 2 3 4)))" |> (expectValue <| Bool True)
            , "(->> [] seq)" |> expectValue Nil
            ]

        -- functions
        , describe "apply"
            [ "(apply + [])" |> (expectValue <| Number <| Int 0)
            , "(apply + [1])" |> (expectValue <| Number <| Int 1)
            , "(apply + [1 2 3])" |> (expectValue <| Number <| Int 6)
            , "(apply + 1 2 [3])" |> (expectValue <| Number <| Int 6)
            ]
        , describe "assoc"
            [ "(assoc nil :a 1)"
                |> (expectValue <| Map <| ValueMap.fromList [ ( Keyword "a", Located.unknown <| Number <| Int 1 ) ])
            , "(assoc {} :a 1 :b 2)"
                |> (expectValue <|
                        Map <|
                            ValueMap.fromList
                                [ ( Keyword "a", Located.unknown <| Number <| Int 1 )
                                , ( Keyword "b", Located.unknown <| Number <| Int 2 )
                                ]
                   )
            , "(assoc {:a 1} :a 2)"
                |> (expectValue <| Map <| ValueMap.fromList [ ( Keyword "a", Located.unknown <| Number <| Int 2 ) ])
            , "(assoc [1 2] 1 3)"
                |> (expectValue <|
                        (Vector <|
                            Array.fromList
                                [ Located.at ( 1, 9 ) ( 1, 11 ) <| Number <| Int 1
                                , Located.unknown <| Number <| Int 3
                                ]
                        )
                   )
            , "(= (assoc [] 0 1) [1])" |> (expectValue <| Bool True)
            , "(= (assoc [] 0 1 1 2 2 3 3 4) [1 2 3 4])" |> (expectValue <| Bool True)
            , "(assoc [] 1 2)" |> (expectException <| "index out of bounds")
            ]
        , describe "assoc-in"
            [ "(= (assoc-in nil [:foo :bar :buz] 42) {:foo {:bar {:buz 42}}})" |> (expectValue <| Bool True)
            , "(= (assoc-in nil [0 1 2] 42) {0 {1 {2 42}}})" |> (expectValue <| Bool True)
            , "(= (assoc-in [] [0 1 2] 42) [{1 {2 42}}])" |> (expectValue <| Bool True)
            , "(assoc-in [] [1 1 2] 42)" |> (expectException <| "index out of bounds")
            , "(= (assoc-in {:foo {} :bar 42} [:foo :buz] 3) {:foo {:buz 3} :bar 42})" |> (expectValue <| Bool True)
            ]
        , describe "concat"
            [ "(= (concat [1] [2 3] [4]) (list 1 2 3 4))" |> (expectValue <| Bool True)
            , "(= (concat [1 2 3] nil nil [4 5]) (list 1 2 3 4 5))" |> (expectValue <| Bool True)
            , "(= (concat [1] () (list 2 3) {4 5}) (list 1 2 3 [4 5]))" |> (expectValue <| Bool True)
            ]
        , describe "conj"
            [ "(= (conj [1 2] 3) [1 2 3])" |> (expectValue <| Bool True)
            , "(= (conj [] 1) [1])" |> (expectValue <| Bool True)
            , "(= (conj [] 1 2 3) [1 2 3])" |> (expectValue <| Bool True)
            , "(= (conj nil 1) (list 1))" |> (expectValue <| Bool True)
            , "(= (conj (list 2 3) 1) (list 1 2 3))" |> (expectValue <| Bool True)
            , "(= (conj (list) 1 2 3) (list 3 2 1))" |> (expectValue <| Bool True)
            , "(= (conj #{} 1 2 3) #{1 2 3})" |> (expectValue <| Bool True)
            , "(= (conj {} [1 2]) {1 2})" |> (expectValue <| Bool True)
            , "(= (conj {} (first {3 4})) {3 4})" |> (expectValue <| Bool True)
            ]
        , describe "cons"
            [ "(= (cons 3 [1 2]) (list 3 1 2))" |> (expectValue <| Bool True)
            , "(= (cons 1 []) (list 1))" |> (expectValue <| Bool True)
            , "(= (cons 1 nil) (list 1))" |> (expectValue <| Bool True)
            , "(= (cons 1 (list 2 3)) (list 1 2 3))" |> (expectValue <| Bool True)
            , "(= (cons 1 #{}) (list 1))" |> (expectValue <| Bool True)
            , "(= (cons [1 2] {}) (list [1 2]))" |> (expectValue <| Bool True)
            , "(= (cons (first {3 4}) {}) (list [3 4]))" |> (expectValue <| Bool True)
            ]
        , describe "contains?"
            [ "(contains? {:a 3} :a)" |> (expectValue <| Bool True)
            , "(contains? {:a 3} :b)" |> (expectValue <| Bool False)
            , "(contains? #{42} 42)" |> (expectValue <| Bool True)
            , "(contains? #{42} 43)" |> (expectValue <| Bool False)
            , "(contains? nil :anything)" |> (expectValue <| Bool False)
            , "(contains? [3 4] 1)" |> (expectValue <| Bool True)
            , "(contains? [3 4] 2)" |> (expectValue <| Bool False)
            , "(contains? nil 2)" |> (expectValue <| Bool False)
            ]
        , describe "constantly"
            [ "((constantly :ret-a))" |> (expectValue <| Keyword "ret-a")
            , "((constantly :ret-b) 1)" |> (expectValue <| Keyword "ret-b")
            , "((constantly :ret-c) 1 2)" |> (expectValue <| Keyword "ret-c")
            ]
        , describe "complement"
            [ "((complement neg?) -3)" |> (expectValue <| Bool False)
            , "((complement seq) [])" |> (expectValue <| Bool True)
            ]
        , describe "dec"
            [ "(dec 1)" |> (expectValue <| Number <| Int 0)
            , "(dec -1)" |> (expectValue <| Number <| Int -2)
            ]
        , describe "dedupe"
            [ "(= (dedupe ()) ())" |> (expectValue <| Bool True)
            , "(= (dedupe []) ())" |> (expectValue <| Bool True)
            , "(= (dedupe #{}) ())" |> (expectValue <| Bool True)
            , "(= (dedupe {}) ())" |> (expectValue <| Bool True)
            , "(= (dedupe [1 2 3 4 5]) (list 1 2 3 4 5))" |> (expectValue <| Bool True)
            , "(= (dedupe [1 1 1 1 1]) (list 1))" |> (expectValue <| Bool True)
            , "(= (dedupe [1 1 1 2 2]) (list 1 2))" |> (expectValue <| Bool True)
            ]
        , describe "drop"
            [ "(drop 0 [])" |> (expectValue <| List [])
            , "(drop 0 nil)" |> (expectValue <| List [])
            , "(drop 100 nil)" |> (expectValue <| List [])
            , "(= (drop 1 [1 2 3]) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (drop 2 [1 2 3]) (list 3))" |> (expectValue <| Bool True)
            , "(= (drop 3 [1 2 3]) (list))" |> (expectValue <| Bool True)
            , "(= (drop 30 [1 2 3]) (list))" |> (expectValue <| Bool True)
            ]
        , describe "drop-while"
            [ "(drop-while odd? [])" |> (expectValue <| List [])
            , "(drop-while odd? nil)" |> (expectValue <| List [])
            , "(drop-while even? nil)" |> (expectValue <| List [])
            , "(= (drop-while odd? [1 3 5 6 8]) (list 6 8))" |> (expectValue <| Bool True)
            , "(= (drop-while odd? [1 2 3]) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (drop-while odd? [2 3]) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (drop-while even? [1 2 3]) (list 1 2 3))" |> (expectValue <| Bool True)
            ]
        , describe "empty?"
            [ "(empty? nil)" |> (expectValue <| Bool True)
            , "(empty? [])" |> (expectValue <| Bool True)
            , "(empty? ())" |> (expectValue <| Bool True)
            , "(empty? {})" |> (expectValue <| Bool True)
            , "(empty? #{})" |> (expectValue <| Bool True)
            , "(empty? [1])" |> (expectValue <| Bool False)
            , "(empty? (list 1))" |> (expectValue <| Bool False)
            , "(empty? {1 2})" |> (expectValue <| Bool False)
            , "(empty? #{1})" |> (expectValue <| Bool False)
            ]
        , describe "even?"
            [ "(even? 2)" |> (expectValue <| Bool True)
            , "(even? 3)" |> (expectValue <| Bool False)
            , "(even? 2.0)" |> (expectException <| "Argument must be an integer: 2")
            ]
        , describe "every?"
            [ "(every? odd? nil)" |> (expectValue <| Bool True)
            , "(every? odd? [])" |> (expectValue <| Bool True)
            , "(every? (comp integer? key) {1 2 3 4 5 6})" |> (expectValue <| Bool True)
            , "(every? odd? [1 3 5])" |> (expectValue <| Bool True)
            , "(every? odd? [1 3 6])" |> (expectValue <| Bool False)
            ]
        , describe "filter"
            [ "(filter pos? nil)" |> (expectValue <| List [])
            , "(= (filter odd? [1 2 3 4 5]) (list 1 3 5))" |> (expectValue <| Bool True)
            ]
        , describe "fnil"
            [ "((fnil inc 0) nil)" |> (expectValue <| Number <| Int 1) ]
        , describe "identity"
            [ "(identity :ret)" |> (expectValue <| Keyword "ret") ]
        , describe "inc"
            [ "(inc 0)" |> (expectValue <| Number <| Int 1)
            , "(inc -1)" |> (expectValue <| Number <| Int 0)
            ]
        , describe "into"
            [ "(= (into [] (list 1 2 3 4)) [1 2 3 4])" |> (expectValue <| Bool True)
            , "(= (into () (list 1 2 3 4)) (list 4 3 2 1))" |> (expectValue <| Bool True)
            , "(= (into {} (list [1 2] [3 4])) {1 2 3 4})" |> (expectValue <| Bool True)
            , "(= (into #{} (list 1 2 3 4)) #{1 2 3 4})" |> (expectValue <| Bool True)
            ]
        , describe "comp"
            [ "((comp) :ret)" |> (expectValue <| Keyword "ret")
            , "((comp inc inc dec) 3)" |> (expectValue <| Number <| Int 4)
            , "((comp #(* 2 %) inc #(* 3 %)) 3)" |> (expectValue <| Number <| Int 20)
            ]
        , describe "dissoc"
            [ "(dissoc {:a 42} :a)" |> (expectValue <| Map ValueMap.empty)
            , "(dissoc {:a nil} :b)"
                |> (expectValue <|
                        Map <|
                            ValueMap.fromList [ ( Keyword "a", Located.at ( 1, 13 ) ( 1, 16 ) Nil ) ]
                   )
            , "(dissoc nil :a)" |> expectValue Nil
            ]
        , describe "first"
            [ "(first [])" |> expectValue Nil
            , "(first [1 2])" |> (expectValue <| Number <| Int 1)
            , "(first nil)" |> expectValue Nil
            , "(first {})" |> expectValue Nil
            , "(first {1 2})"
                |> (expectValue <|
                        MapEntry ( Number <| Int 1, Located.at ( 1, 11 ) ( 1, 12 ) <| Number <| Int 2 )
                   )
            , "(first #{})" |> expectValue Nil
            , "(first #{2})" |> (expectValue <| Number <| Int 2)
            , "(first (list))" |> expectValue Nil
            , "(first (list 1 2 3))" |> (expectValue <| Number <| Int 1)
            ]
        , describe "float?"
            [ "(float? 42.0)" |> (expectValue <| Bool True)
            , "(float? 42)" |> (expectValue <| Bool False)
            , "(float? :float)" |> (expectValue <| Bool False)
            ]
        , describe "get"
            [ "(get nil :something)" |> (expectValue <| Nil)
            , "(get nil :something :anything)" |> (expectValue <| Keyword "anything")
            , "(get {:a 42} :a)" |> (expectValue <| Number <| Int 42)
            , "(get {:a 42} :b)" |> expectValue Nil
            , "(get {} :b :default)" |> (expectValue <| Keyword "default")
            , "(get {:b nil} :b :default)" |> expectValue Nil
            , "(get [] 0)" |> expectValue Nil
            , "(get [] 0 :default)" |> (expectValue <| Keyword "default")
            , "(get [1] 0 :default)" |> (expectValue <| Number <| Int 1)
            , "(get #{} 0)" |> expectValue Nil
            , "(get #{} 0 :default)" |> (expectValue <| Keyword "default")
            , "(get #{0} 0 :default)" |> (expectValue <| Number <| Int 0)
            ]
        , describe "get-in"
            [ "(get-in nil [:a 0 :b])" |> expectValue Nil
            , "(get-in nil [:a])" |> expectValue Nil
            , "(get-in nil [])" |> expectValue Nil
            , "(get-in {} [:a 0 :b])" |> expectValue Nil
            , "(get-in {} [:a])" |> expectValue Nil
            , "(get-in {} [])" |> (expectValue <| Map <| ValueMap.empty)
            , "(get-in [3 [{:foo 2}]] [1 0 :foo])" |> (expectValue <| Number <| Int 2)
            , "(get-in [3 [{:foo 2}]] [0])" |> (expectValue <| Number <| Int 3)
            , "(get-in [] [4])" |> expectValue Nil
            , "(get-in [] [:foo])" |> expectValue Nil
            , "(get-in {:foo {:bar {:buz [0 1 2 3]}}} [:foo :bar :buz 3])" |> (expectValue <| Number <| Int 3)
            ]
        , describe "json/encode"
            [ "(json/encode {})" |> (expectValue <| String "{}")
            , "(json/encode {\"key\" \"value\"})" |> (expectValue <| String "{\"key\":\"value\"}")
            , "(json/encode [])" |> (expectValue <| String "[]")
            , "(json/encode [{} {}])" |> (expectValue <| String "[{},{}]")
            , "(json/encode ())" |> (expectValue <| String "[]")
            , "(json/encode 42)" |> (expectValue <| String "42")
            , "(json/encode 42.5)" |> (expectValue <| String "42.5")
            , "(json/encode \"\")" |> (expectValue <| String "\"\"")
            , "(json/encode :kw)" |> (expectValue <| String "\"kw\"")
            , "(json/encode nil)" |> (expectValue <| String "null")
            , "(json/encode true)" |> (expectValue <| String "true")
            , "(json/encode false)" |> (expectValue <| String "false")
            , "(json/encode (first {\"key\" \"value\"}))" |> (expectValue <| String "[\"key\",\"value\"]")
            , "(json/encode #{})" |> (expectValue <| String "[]")
            , "(json/encode #{1})" |> (expectValue <| String "[1]")
            ]
        , describe "json/decode"
            [ "(json/decode \"[]\")" |> (expectValue <| Vector <| Array.empty)
            , "(json/decode \"{}\")" |> (expectValue <| Map <| ValueMap.empty)
            , "(json/decode \"\\\"foo\\\"\")" |> (expectValue <| String "foo")
            , "(json/decode \"null\")" |> (expectValue <| Nil)
            , "(json/decode \"true\")" |> (expectValue <| Bool True)
            , "(json/decode \"false\")" |> (expectValue <| Bool False)
            , "(json/decode \"42\")" |> (expectValue <| Number <| Int 42)
            , "(json/decode \"42.5\")" |> (expectValue <| Number <| Float 42.5)
            ]
        , describe "integer?"
            [ "(integer? 42)" |> (expectValue <| Bool True)
            , "(integer? 42.5)" |> (expectValue <| Bool False)
            , "(integer? \"42\")" |> (expectValue <| Bool False)
            , "(integer? nil)" |> (expectValue <| Bool False)
            ]
        , describe "key"
            [ "(key (first {1 2}))" |> (expectValue <| Number <| Int 1) ]
        , describe "last"
            [ "(last nil)" |> expectValue Nil
            , "(last [])" |> expectValue Nil
            , "(last #{})" |> expectValue Nil
            , "(last ())" |> expectValue Nil
            , "(last [:foo :bar :ret])" |> (expectValue <| Keyword "ret")
            , "(last (list :foo :bar :ret))" |> (expectValue <| Keyword "ret")
            ]
        , describe "list"
            [ "(list nil nil)" |> (expectValue <| List [ Located.unknown Nil, Located.unknown Nil ])
            , "(list)" |> (expectValue <| List [])
            ]
        , describe "mod"
            [ "(mod 5 2)" |> (expectValue <| Number <| Int 1)
            , "(mod 7 -2)" |> (expectValue <| Number <| Int -1)
            , "(mod -9 -2)" |> (expectValue <| Number <| Int -1)
            ]
        , describe "Exception."
            [ "(Exception. \"error\")" |> (expectValue <| Throwable <| Exception "error" []) ]
        , describe "map"
            [ "(map inc nil)" |> (expectValue <| List [])
            , "(= (map inc [1 2 3]) (list 2 3 4))" |> (expectValue <| Bool True)
            , "(= (map #(key %) {1 2}) (list 1))" |> (expectValue <| Bool True)
            ]
        , describe "mapcat"
            [ "(= (mapcat (fn [i] [i i]) [1 2 3]) (list 1 1 2 2 3 3))" |> (expectValue <| Bool True)
            , "(= (mapcat identity [[1 2] nil [3 4]]) (list 1 2 3 4))" |> (expectValue <| Bool True)
            ]
        , describe "map-indexed"
            [ "(= (map-indexed (fn [i e] [i e]) [1 2 3]) (list [0 1] [1 2] [2 3]))" |> (expectValue <| Bool True) ]
        , describe "neg?"
            [ "(neg? 3)" |> (expectValue <| Bool False)
            , "(neg? 0)" |> (expectValue <| Bool False)
            , "(neg? -10)" |> (expectValue <| Bool True)
            ]
        , describe "next"
            [ "(next nil)" |> expectValue Nil
            , "(next #{})" |> expectValue Nil
            , "(next ())" |> expectValue Nil
            , "(next [])" |> expectValue Nil
            , "(next {})" |> expectValue Nil
            , "(next [1])" |> expectValue Nil
            , "(next #{1}))" |> expectValue Nil
            , "(next (list 1))" |> expectValue Nil
            , "(= (next [1 2 3]) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (next (list 1 2 3)) (list 2 3))" |> (expectValue <| Bool True)
            ]
        , describe "nil?"
            [ "(nil? nil)" |> (expectValue <| Bool True)
            , "(nil? false)" |> (expectValue <| Bool False)
            , "(nil? \"nil\")" |> (expectValue <| Bool False)
            ]
        , describe "not"
            [ "(not true)" |> (expectValue <| Bool False)
            , "(not false)" |> (expectValue <| Bool True)
            , "(not nil)" |> (expectValue <| Bool True)
            , "(not 42)" |> (expectValue <| Bool False)
            ]
        , describe "number?"
            [ "(number? 42)" |> (expectValue <| Bool True)
            , "(number? 42.5)" |> (expectValue <| Bool True)
            , "(number? \"42.5\")" |> (expectValue <| Bool False)
            , "(number? nil)" |> (expectValue <| Bool False)
            ]
        , describe "odd?"
            [ "(odd? 2)" |> (expectValue <| Bool False)
            , "(odd? 3)" |> (expectValue <| Bool True)
            , "(odd? 2.0)" |> (expectException <| "Argument must be an integer: 2")
            ]
        , describe "peek"
            [ "(peek (list 1 2 3))" |> (expectValue <| Number <| Int 1)
            , "(peek [1 2 3])" |> (expectValue <| Number <| Int 3)
            , "(peek nil)" |> expectValue Nil
            ]
        , describe "pos?"
            [ "(pos? 3)" |> (expectValue <| Bool True)
            , "(pos? 0)" |> (expectValue <| Bool False)
            , "(pos? -10)" |> (expectValue <| Bool False)
            ]
        , describe "pr-str"
            [ "(pr-str 42)" |> (expectValue <| String "42")
            , "(pr-str 42.5)" |> (expectValue <| String "42.5")
            , "(pr-str pr-str)" |> (expectValue <| String "fn<pr-str>")
            , "(pr-str ())" |> (expectValue <| String "()")
            , "(pr-str [])" |> (expectValue <| String "[]")
            , "(pr-str nil)" |> (expectValue <| String "nil")
            , "(pr-str false)" |> (expectValue <| String "false")
            , "(pr-str true)" |> (expectValue <| String "true")
            , "(pr-str :keyword)" |> (expectValue <| String ":keyword")
            , "(pr-str {})" |> (expectValue <| String "{}")
            , "(pr-str (first {1 2}))" |> (expectValue <| String "[1 2]")
            , "(pr-str #{})" |> (expectValue <| String "#{}")
            ]
        , describe "reduce"
            [ "(reduce + [])" |> (expectValue <| Number <| Int 0)
            , "(reduce + 2 [])" |> (expectValue <| Number <| Int 2)
            , "(reduce + [1 2 3 4 5])" |> (expectValue <| Number <| Int 15)
            , "(reduce + 3 [1 2 3 4 5])" |> (expectValue <| Number <| Int 18)
            ]
        , describe "reduce-kv"
            [ "(= (reduce-kv (fn [a k v] (conj a k v)) #{} {1 2 3 4 5 6}) #{1 2 3 4 5 6})" |> (expectValue <| Bool True) ]
        , describe "rem"
            [ "(rem 8 3)" |> (expectValue <| Number <| Int 2)
            , "(rem 8 2)" |> (expectValue <| Number <| Int 0)
            , "(rem 8.0 3.0)" |> (expectValue <| Number <| Float 2.0)
            , "(rem 8.0 2.0)" |> (expectValue <| Number <| Float 0.0)
            , "(rem 8.0 3)" |> (expectValue <| Number <| Float 2.0)
            , "(rem 8 3.0)" |> (expectValue <| Number <| Float 2.0)
            ]
        , describe "remove"
            [ "(remove neg? nil)" |> (expectValue <| List [])
            , "(= (remove even? [1 2 3 4 5]) (list 1 3 5))" |> (expectValue <| Bool True)
            ]
        , describe "repeat"
            [ "(= (repeat 0 nil) ())" |> (expectValue <| Bool True)
            , "(= (repeat -1 nil) ())" |> (expectValue <| Bool True)
            , "(= (repeat 1 nil) (list nil))" |> (expectValue <| Bool True)
            , "(= (repeat 5 1) (list 1 1 1 1 1))" |> (expectValue <| Bool True)
            ]
        , describe "rest"
            [ "(= (rest (list 1 2 3)) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (rest [1 2 3]) (list 2 3))" |> (expectValue <| Bool True)
            , "(= (rest nil) (list))" |> (expectValue <| Bool True)
            ]
        , describe "reverse"
            [ "(= (reverse [1 2 3]) (list 3 2 1))" |> (expectValue <| Bool True)
            , "(= (reverse [4 3 2]) (list 2 3 4))" |> (expectValue <| Bool True)
            , "(reverse nil)" |> (expectValue <| List [])
            ]
        , describe "second"
            [ "(second nil)" |> expectValue Nil
            , "(second (list 1 2 3))" |> (expectValue <| Number <| Int 2)
            , "(second [1 2 3])" |> (expectValue <| Number <| Int 2)
            , "(second ())" |> expectValue Nil
            , "(second [1])" |> expectValue Nil
            , "(second #{})" |> expectValue Nil
            , "(second {})" |> expectValue Nil
            , "(second (first {1 2}))" |> (expectValue <| Number <| Int 2)
            ]
        , describe "seq"
            [ "(seq nil)" |> expectValue Nil
            , "(seq [])" |> expectValue Nil
            , "(seq ())" |> expectValue Nil
            , "(seq {})" |> expectValue Nil
            , "(seq #{})" |> expectValue Nil
            , "(= (seq [1]) (list 1))" |> (expectValue <| Bool True)
            , "(= (seq (list 1)) (list 1))" |> (expectValue <| Bool True)
            , "(= (seq {1 2}) (list [1 2]))" |> (expectValue <| Bool True)
            , "(= (seq #{1}) (list 1))" |> (expectValue <| Bool True)
            ]
        , describe "some"
            [ "(some nil nil)" |> expectValue Nil
            , "(some pos? nil)" |> expectValue Nil
            , "(some pos? [])" |> expectValue Nil
            , "(some pos? ())" |> expectValue Nil
            , "(some pos? {})" |> expectValue Nil
            , "(some pos? #{})" |> expectValue Nil
            , "(some pos? [0 2])" |> (expectValue <| Bool True)
            , "(some pos? [0])" |> expectValue Nil
            , "(some #{5 4} [0 1 2 5])" |> (expectValue <| Number <| Int 5)
            ]
        , describe "str"
            [ "(str)" |> (expectValue <| String "")
            , "(str 42)" |> (expectValue <| String "42")
            , "(str 42.3)" |> (expectValue <| String "42.3")
            , "(str \"1\")" |> (expectValue <| String "1")
            , "(str \"1\" \"2\" \"3\" \"4\" \"5\")" |> (expectValue <| String "12345")
            , "(str str)" |> (expectValue <| String "fn<str>")
            , "(str ())" |> (expectValue <| String "()")
            , "(str [])" |> (expectValue <| String "[]")
            , "(str nil)" |> (expectValue <| String "")
            , "(str false)" |> (expectValue <| String "false")
            , "(str true)" |> (expectValue <| String "true")
            , "(str :keyword)" |> (expectValue <| String ":keyword")
            , "(str {})" |> (expectValue <| String "{}")
            , "(str (first {1 2}))" |> (expectValue <| String "[1 2]")
            , "(str #{})" |> (expectValue <| String "#{}")
            ]
        , describe "string/join"
            [ "(string/join nil)" |> (expectValue <| String "")
            , "(string/join [\"1\" \"2\" \"3\"])" |> (expectValue <| String "123")
            , "(string/join [{} {} {}])" |> (expectValue <| String "{}{}{}")
            , "(string/join \", \" [\"1\" \"2\" \"3\"])" |> (expectValue <| String "1, 2, 3")
            , "(string/join \", \" [1 2 3])" |> (expectValue <| String "1, 2, 3")
            , "(string/join \", \" nil)" |> (expectValue <| String "")
            ]
        , describe "string/length"
            [ "(string/length \"butterfly\")" |> (expectValue <| Number <| Int <| 9)
            , "(string/length \"\")" |> (expectValue <| Number <| Int <| 0)
            ]
        , describe "string/split-lines"
            [ "(= (string/split-lines \"a\\nb\\nc\") (list \"a\" \"b\" \"c\"))" |> (expectValue <| Bool True)
            , "(= (string/split-lines \"a\\n\\nb\") (list \"a\" \"\" \"b\"))" |> (expectValue <| Bool True)
            , "(= (string/split-lines \"\") (list \"\"))" |> (expectValue <| Bool True)
            ]
        , describe "take"
            [ "(take 0 [])" |> (expectValue <| List [])
            , "(take 0 nil)" |> (expectValue <| List [])
            , "(take 100 nil)" |> (expectValue <| List [])
            , "(= (take 1 [1 2 3]) (list 1))" |> (expectValue <| Bool True)
            , "(= (take 2 [1 2 3]) (list 1 2))" |> (expectValue <| Bool True)
            , "(= (take 3 [1 2 3]) (list 1 2 3))" |> (expectValue <| Bool True)
            , "(= (take 30 [1 2 3]) (list 1 2 3))" |> (expectValue <| Bool True)
            ]
        , describe "take-while"
            [ "(take-while odd? [])" |> (expectValue <| List [])
            , "(take-while odd? nil)" |> (expectValue <| List [])
            , "(take-while even? nil)" |> (expectValue <| List [])
            , "(= (take-while odd? [1 3 5 6 8]) (list 1 3 5))" |> (expectValue <| Bool True)
            , "(= (take-while odd? [1 2 3]) (list 1))" |> (expectValue <| Bool True)
            , "(= (take-while odd? [2 3]) ())" |> (expectValue <| Bool True)
            , "(= (take-while even? [1 2 3]) ())" |> (expectValue <| Bool True)
            ]
        , describe "throw"
            [ "(throw (Exception. \"hi\"))" |> (expectException <| "hi")
            , "(throw nil)" |> (expectException <| "nil is not throwable")
            ]
        , describe "update"
            [ "(= (update nil :foo assoc :bar 2) {:foo {:bar 2}})" |> (expectValue <| Bool True)
            , "(:foo (update {:foo 42} :foo inc))" |> (expectValue <| Number <| Int 43)
            ]
        , describe "update-in"
            [ "(= (update-in nil [:a :b] (constantly :foo)) {:a {:b :foo}})" |> (expectValue <| Bool True)
            , "(= (update-in nil [] (constantly :foo)) {nil :foo})" |> (expectValue <| Bool True)
            , "(= (update-in {:foo [1 2]} [:foo 1] inc) {:foo [1 3]})" |> (expectValue <| Bool True)
            ]
        , describe "update-vals"
            [ "(= (update-vals {:foo 1 :bar 2} inc) {:foo 2 :bar 3}))" |> (expectValue <| Bool True)
            ]
        , describe "val"
            [ "(val (first {1 2}))" |> (expectValue <| Number <| Int 2)
            ]
        , describe "zero?"
            [ "(zero? 3)" |> (expectValue <| Bool False)
            , "(zero? 0)" |> (expectValue <| Bool True)
            , "(zero? -10)" |> (expectValue <| Bool False)
            ]
        , describe "stack traces"
            [ """
                 (do
                  (throw (Exception. "foo")))
                 """
                |> expectExceptionWithTrace "foo" [ "throw:3", "user:3" ]
            , """
              (defn a [x] (throw (Exception. x)))
              (defn b [x] (a x))
              (defn c [x] (b x))
              (c "foo")
              """
                |> expectExceptionWithTrace "foo" [ "throw:2", "a:2", "b:3", "c:4", "user:5" ]
            , """
              (defn a [n]
                (if (< 0 n)
                  (a (dec n))
                  (throw (Exception. "done"))))
              (a 5)
              """
                |> expectExceptionWithTrace "done" [ "throw:5", "a:5", "a:4", "a:4", "a:4", "a:4", "a:4", "user:6" ]
            , """
              (defn a [n]
                (+ n "str"))
              (a 1)
              """
                |> expectExceptionWithTrace "\"str\" is not a number" [ "+:3", "a:3", "user:4" ]
            ]
        ]