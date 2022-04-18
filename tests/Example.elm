module Example exposing (..)

import Array
import Enclojure
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Types exposing (Env, Exception(..), Number(..), Value(..))
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


eval : String -> Result Exception Value
eval code =
    Enclojure.evalPure Enclojure.init code
        |> Result.mapError Located.getValue
        |> Result.map Tuple.first


suite : Test
suite =
    describe "Enclojure"
        ([ ( "booleans"
           , [ ( "true", Ok <| Bool True )
             , ( "false", Ok <| Bool False )
             ]
           )
         , ( "nils"
           , [ ( "nil", Ok Nil ) ]
           )
         , ( "numbers"
           , [ ( "42", Ok <| Number <| Int 42 )
             , ( "42.0", Ok <| Number <| Float 42.0 )
             ]
           )
         , ( "strings"
           , [ ( "\"\"", Ok <| String "" )
             , ( "\"", Err <| Exception "Missing closing quote for a string at row 1, col 2" )
             , ( "\"foo\"", Ok <| String "foo" )
             , ( "\"\\n\"", Ok <| String "\n" )
             , ( "\"\\t\"", Ok <| String "\t" )
             , ( "\"\\\\\"", Ok <| String "\\" )
             , ( "\"\\\"\"", Ok <| String "\"" )
             ]
           )
         , ( "keywords"
           , [ ( ":foo", Ok <| Keyword "foo" )
             , ( ":a-b$c_d?e+f/g*h>i<j=k'l&m%n?o.p$q_", Ok <| Keyword "a-b$c_d?e+f/g*h>i<j=k'l&m%n?o.p$q_" )
             , ( "(:foo {:foo 42})", Ok <| Number <| Int 42 )
             , ( "(:foo nil :bar)", Ok <| Keyword "bar" )
             , ( "(:foo {})", Ok Nil )
             , ( "(:foo #{})", Ok Nil )
             , ( "(:foo #{:foo})", Ok <| Keyword "foo" )
             ]
           )
         , ( "lists"
           , [ ( "()", Ok <| List [] )
             ]
           )
         , ( "vectors"
           , [ ( "[]", Ok <| Vector Array.empty ) ]
           )
         , ( "maps"
           , [ ( "{}", Ok <| Map ValueMap.empty )
             , ( "({:a 1} :a)", Ok <| Number <| Int 1 )
             , ( "({\"a\" 2} \"a\")", Ok <| Number <| Int 2 )
             , ( "({[] 3} [])", Ok <| Number <| Int 3 )
             , ( "({nil 4} nil)", Ok <| Number <| Int 4 )
             , ( "({true 5} true)", Ok <| Number <| Int 5 )
             , ( "({false 6} false)", Ok <| Number <| Int 6 )
             , ( "({7 7} 7)", Ok <| Number <| Int 7 )
             , ( "({8.0 8} 8.0)", Ok <| Number <| Int 8 )
             , ( "({() 9} ())", Ok <| Number <| Int 9 )
             , ( "({{} 10} {})", Ok <| Number <| Int 10 )
             ]
           )
         , ( "sets"
           , [ ( "#{}", Ok <| Set ValueSet.empty )
             , ( "(= #{42 42 42} #{42})", Ok <| Bool True )
             , ( "(#{} 42)", Ok Nil )
             , ( "(#{false} false)", Ok <| Bool False )
             , ( "(#{true} true)", Ok <| Bool True )
             , ( "(#{nil} nil)", Ok Nil )
             , ( "(#{42} 42)", Ok <| Number <| Int 42 )
             , ( "(#{42.0} 42.0)", Ok <| Number <| Float 42.0 )
             , ( "(#{\"a\"} \"a\")", Ok <| String "a" )
             , ( "(#{:a} :a)", Ok <| Keyword "a" )
             , ( "(#{[]} [])", Ok <| Vector Array.empty )
             , ( "(#{{}} {})", Ok <| Map ValueMap.empty )
             , ( "(#{()} ())", Ok <| List [] )
             , ( "(#{#{}} #{})", Ok <| Set ValueSet.empty )
             ]
           )

         -- special forms
         , ( "def"
           , [ ( "(def foo 42) foo", Ok <| Number <| Int 42 )
             , ( "(def foo 42) (def foo 43) foo", Ok <| Number <| Int 43 )
             ]
           )
         , ( "do"
           , [ ( "(do)", Ok Nil )
             , ( "(do :foo)", Ok <| Keyword "foo" )
             , ( """
                 (do
                  (def a 1)
                  (def b 2)
                  (def c 3)
                  #{a b c})
                 """
               , Ok <| Set (ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Number <| Int 3 ])
               )
             ]
           )
         , ( "if"
           , [ ( "(if true 42)", Ok <| Number <| Int 42 )
             , ( "(if false 42)", Ok Nil )
             , ( "(if true 42 43)", Ok <| Number <| Int 42 )
             , ( "(if false 42 43)", Ok <| Number <| Int 43 )
             , ( "(if true)", Err <| Exception "an if without then" )
             , ( "(if true (def a 1) (def b 2)) b", Err <| Exception "Unknown symbol b" )
             , ( "(if true (def a 1) (def b 2)) a", Ok <| Number <| Int 1 )
             ]
           )
         , ( "quote"
           , [ ( "(quote foo)", Ok <| Symbol "foo" )
             , ( "(= (quote (1 2 3)) (list 1 2 3))", Ok <| Bool True )
             ]
           )
         , ( "let"
           , [ ( "(let [a 1 b 2 c 3] (+ a b c))", Ok <| Number <| Int 6 )
             , ( "(let [_ (def a 1) _ (def a 2)] a)", Ok <| Number <| Int 2 )
             , ( "(let [[a b c] [2 3 4]] (+ a b c))", Ok <| Number <| Int 9 )
             , ( "(let [[a & rst] [3 4 5]] (+ a (first rst) (second rst)))", Ok <| Number <| Int 12 )
             , ( "(let [[a & [b c]] [4 5 6]] (+ a b c))", Ok <| Number <| Int 15 )
             , ( "(let [{a :a} {:a 1}] a)", Ok <| Number <| Int 1 )
             , ( "(let [{a :a} {}] a)", Ok Nil )
             , ( "(let [{a 1} [2 3 4]] a)", Ok <| Number <| Int 3 )
             , ( "(let [{a 1} []] a)", Ok Nil )
             , ( "(let [{a :a :or {a 2}} {}] a)", Err <| Exception ":or is not supported" )
             , ( "(let [[:as foo] nil] foo)", Ok Nil )
             , ( "(let [[:as foo] []] foo)", Ok <| Vector <| Array.empty )
             , ( """
                 (let [[a b & rst :as all] [1 2 3 4 5]]
                  (= [a b rst all]
                     [1 2 (list 3 4 5) [1 2 3 4 5]]))
                 """
               , Ok <| Bool True
               )
             , ( "(let [{:keys [a b c]} {:a 1 :b 2}] #{a b c})"
               , Ok <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Nil ]
               )
             , ( "(let [{:keys [a]} {\"a\" 3}] a)", Ok Nil )
             , ( "(let [{:keys [a]} nil] a)", Ok Nil )
             , ( "(let [{:keys [a]} []] a)", Ok Nil )
             , ( "(let [{:strs [a b c]} {:a 1 :b 2}] #{a b c})"
               , Ok <| Set <| ValueSet.fromList [ Nil, Nil, Nil ]
               )
             , ( "(let [{:strs [a b c]} {\"a\" 1 \"b\" 2}] #{a b c})"
               , Ok <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Nil ]
               )
             , ( "(let [{:strs [a]} nil] a)", Ok Nil )
             , ( "(let [{:strs [a]} []] a)", Ok Nil )
             , ( "(let [{:strs [a]} {}] a)", Ok Nil )
             ]
           )
         , ( "fn"
           , [ ( "((fn [] :a))", Ok <| Keyword "a" )
             , ( "((fn [] :a :b :c))", Ok <| Keyword "c" )
             , ( "((fn [a] a) :b)", Ok <| Keyword "b" )
             , ( "((fn [a b] #{a b}) :a :b)", Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b" ] )
             , ( "((fn [a b c] #{a b c}) :a :b :c)"
               , Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c" ]
               )
             , ( "((fn [a b c d] #{a b c d}) :a :b :c :d)"
               , Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d" ]
               )
             , ( "((fn [a b c d e] #{a b c d e}) :a :b :c :d :e)"
               , Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d", Keyword "e" ]
               )
             , ( "((fn [& args] (into #{} args)) :a :b :c :d :e)"
               , Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d", Keyword "e" ]
               )
             , ( "((fn [a & rst] (into #{} (cons a rst))) :a :b :c :d)"
               , Ok <| Set <| ValueSet.fromList [ Keyword "a", Keyword "b", Keyword "c", Keyword "d" ]
               )
             , ( "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))))", Ok <| Number <| Int 42 )
             , ( "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))) 43)", Ok <| Number <| Int 43 )
             , ( "((fn ([] 42) ([a] a) ([a & rst] (into #{} rst))) 44 45 46)"
               , Ok <| Set <| ValueSet.fromList [ Number <| Int 45, Number <| Int 46 ]
               )
             , ( "((fn [n] (if (pos? n) (+ n (recur (dec n))) 0)) 10)", Ok <| Number <| Int 55 )
             , ( "((fn foo [n] (if (pos? n) (+ n (foo (dec n))) 0)) 10)", Ok <| Number <| Int 55 )
             ]
           )
         , ( "#()"
           , [ ( "(#(do :result))", Ok <| Keyword "result" )
             , ( "(#(inc %) 1)", Ok <| Number <| Int 2 )
             , ( "(#(inc %1) 1)", Ok <| Number <| Int 2 )
             , ( "(#(inc %2) 1)", Err <| Exception "Argument error: Too few arguments" )
             , ( "(#(+ % %2) 1 2)", Ok <| Number <| Int 3 )
             , ( "(#(+ %1 %2) 1 2)", Ok <| Number <| Int 3 )
             , ( "(#(+ %1 (last %&)) 2 3 4)", Ok <| Number <| Int 6 )
             , ( "(#(into #{} %&) 1 2 3)"
               , Ok <| Set <| ValueSet.fromList [ Number <| Int 1, Number <| Int 2, Number <| Int 3 ]
               )
             , ( "#(map #(inc %) %)"
               , Err (Exception "Parsing error: nested #() are not supported, use fn instead. at row 1, col 18")
               )
             ]
           )

         -- operators
         , ( "+"
           , [ ( "(+)", Ok <| Number <| Int 0 )
             , ( "(+ 1)", Ok <| Number <| Int 1 )
             , ( "(+ 1 2)", Ok <| Number <| Int 3 )
             , ( "(+ 1 2 3 4 5)", Ok <| Number <| Int 15 )
             , ( "(+ 1 0.2)", Ok <| Number <| Float 1.2 )
             , ( "(+ 0.8 0.2)", Ok <| Number <| Float 1.0 )
             ]
           )
         , ( "-"
           , [ ( "(-)", Err (Exception "Invalid arity 0") )
             , ( "(- 1)", Ok <| Number <| Int -1 )
             , ( "(- 1 2)", Ok <| Number <| Int -1 )
             , ( "(- 1 2 3 4 5)", Ok <| Number <| Int -13 )
             ]
           )
         , ( "*"
           , [ ( "(*)", Ok <| Number <| Int 1 )
             , ( "(* 2)", Ok <| Number <| Int 2 )
             , ( "(* 2 3)", Ok <| Number <| Int 6 )
             , ( "(* 1 2 3 4 5)", Ok <| Number <| Int 120 )
             , ( "(* 2 1.5)", Ok <| Number <| Float 3.0 )
             ]
           )
         , ( "/"
           , [ ( "(/)", Err (Exception "Invalid arity 0") )
             , ( "(/ 2)", Ok <| Number <| Int 0 )
             , ( "(/ 4 2)", Ok <| Number <| Int 2 )
             , ( "(/ 16 2 2)", Ok <| Number <| Int 4 )
             , ( "(/ 5 2)", Ok <| Number <| Int 2 )
             , ( "(/ 5.0 2)", Ok <| Number <| Float 2.5 )
             , ( "(/ 5 2.0)", Ok <| Number <| Float 2.5 )
             ]
           )
         , ( "="
           , [ ( "(=)", Err (Exception "Invalid arity 0") )
             , ( "(= nil)", Ok <| Bool True )
             , ( "(= 1 1)", Ok <| Bool True )
             , ( "(= 1 2)", Ok <| Bool False )
             ]
           )
         , ( "not="
           , [ ( "(not=)", Err (Exception "Invalid arity 0") )
             , ( "(not= nil)", Ok <| Bool False )
             , ( "(not= 1 1)", Ok <| Bool False )
             , ( "(not= 1 2)", Ok <| Bool True )
             ]
           )
         , ( ">"
           , [ ( "(>)", Err (Exception "Invalid arity 0") )
             , ( "(> 1)", Ok <| Bool True )
             , ( "(> 1 1)", Ok <| Bool False )
             , ( "(> 2 1)", Ok <| Bool True )
             , ( "(> 1 2)", Ok <| Bool False )
             , ( "(> \"a\" \"b\")", Ok <| Bool False )
             , ( "(> 4.0 2)", Ok <| Bool True )
             , ( "(> 2.0 4.0)", Ok <| Bool False )
             ]
           )
         , ( ">="
           , [ ( "(>=)", Err (Exception "Invalid arity 0") )
             , ( "(>= 1)", Ok <| Bool True )
             , ( "(>= 1 1)", Ok <| Bool True )
             , ( "(>= 2 1)", Ok <| Bool True )
             , ( "(>= 1 2)", Ok <| Bool False )
             , ( "(>= \"a\" \"b\")", Ok <| Bool False )
             , ( "(>= \"a\" \"a\")", Ok <| Bool True )
             , ( "(>= 4.0 2)", Ok <| Bool True )
             , ( "(>= 2.0 4.0)", Ok <| Bool False )
             ]
           )
         , ( "<"
           , [ ( "(<)", Err (Exception "Invalid arity 0") )
             , ( "(< 1)", Ok <| Bool True )
             , ( "(< 1 1)", Ok <| Bool False )
             , ( "(< 2 1)", Ok <| Bool False )
             , ( "(< 1 2)", Ok <| Bool True )
             , ( "(< \"a\" \"b\")", Ok <| Bool True )
             , ( "(< 4.0 2)", Ok <| Bool False )
             , ( "(< 2.0 4.0)", Ok <| Bool True )
             ]
           )
         , ( "<="
           , [ ( "(<=)", Err (Exception "Invalid arity 0") )
             , ( "(<= 1)", Ok <| Bool True )
             , ( "(<= 1 1)", Ok <| Bool True )
             , ( "(<= 2 1)", Ok <| Bool False )
             , ( "(<= 1 2)", Ok <| Bool True )
             , ( "(<= \"a\" \"b\")", Ok <| Bool True )
             , ( "(<= \"a\" \"a\")", Ok <| Bool True )
             , ( "(<= 4.0 2)", Ok <| Bool False )
             , ( "(<= 2.0 4.0)", Ok <| Bool True )
             ]
           )

         -- built-in macros
         , ( "and"
           , [ ( "(and :foo :bar)", Ok <| Keyword "bar" )
             , ( "(and (def a :foo) (def b :bar) #{a b})"
               , Ok <| Set <| ValueSet.fromList [ Keyword "foo", Keyword "bar" ]
               )
             , ( "(and nil :bar)", Ok Nil )
             , ( "(and true true true true true false :foo)", Ok <| Bool False )
             ]
           )
         , ( "cond"
           , [ ( "(cond (< 2 4) :foo)", Ok <| Keyword "foo" )
             , ( "(cond (< 4 2) :foo (pos? 0) :bar (zero? 0) :buz)", Ok <| Keyword "buz" )
             , ( "(cond (< 4 2) :foo :let [bar 42] (pos? bar) bar)", Ok <| Number <| Int 42 )
             , ( "(cond (< 4 2) :foo :else :else)", Ok <| Keyword "else" )
             ]
           )
         , ( "defn"
           , [ ( """
                 (defn foo
                   [n]
                   n)

                 (foo :ret)
                 """
               , Ok <| Keyword "ret"
               )
             , ( """
                 (defn weird-plus
                   ([n]
                    (weird-plus n 2))
                   ([a b]
                    (+ a b)))

                 (+ (weird-plus 4) (weird-plus 6 1))
                 """
               , Ok <| Number <| Int 13
               )
             ]
           )
         , ( "if-let"
           , [ ( "(if-let [a nil] a :else)", Ok <| Keyword "else" )
             , ( "(if-let [a :then] a :else)", Ok <| Keyword "then" )
             , ( "(if-let [a :then] a :else :or-else)", Err <| Exception "an if with too many forms" )
             ]
           )
         , ( "or"
           , [ ( "(or)", Ok Nil )
             , ( "(or false)", Ok Nil )
             , ( "(or false false false false true)", Ok <| Bool True )
             , ( "(or true (def a 42)) a", Err <| Exception "Unknown symbol a" )
             ]
           )
         , ( "some->"
           , [ ( "(some-> :ret)", Ok <| Keyword "ret" )
             , ( "(some-> false)", Ok <| Bool False )
             , ( "(some-> nil inc)", Ok Nil )
             , ( "(some-> 0 inc inc)", Ok <| Number <| Int 2 )
             , ( "(some-> [] seq inc)", Ok Nil )
             ]
           )
         , ( "some->>"
           , [ ( "(some->> :ret)", Ok <| Keyword "ret" )
             , ( "(some->> false)", Ok <| Bool False )
             , ( "(some->> [1] (map inc) (reduce +)))", Ok <| Number <| Int 2 )
             , ( "(some->> [1] (map inc) (drop 1))", Ok <| List [] )
             , ( "(some->> [1] (map inc) (drop 1) seq (cons 1))", Ok Nil )
             , ( "(some->> [1] (map inc) (drop 1) (cons 1) (into #{}))"
               , Ok <| Set <| ValueSet.fromList [ Number <| Int 1 ]
               )
             ]
           )
         , ( "when"
           , [ ( "(when true 1 2 3 4)", Ok <| Number <| Int 4 )
             , ( "(when true (def a 1) (def b 2) (def c 3) (+ a b c))", Ok <| Number <| Int 6 )
             , ( "(when false 42)", Ok Nil )
             ]
           )
         , ( "when-let"
           , [ ( "(when-let [a nil] 42 43)", Ok Nil )
             , ( "(when-let [a :ret] 42 a)", Ok <| Keyword "ret" )
             , ( "(when-let [a 3] (def b 2) (+ a b))", Ok <| Number <| Int 5 )
             ]
           )
         , ( "when-not"
           , [ ( "(when-not false 1 2 3 4)", Ok <| Number <| Int 4 )
             , ( "(when-not false (def a 1) (def b 2) (def c 3) (+ a b c))", Ok <| Number <| Int 6 )
             , ( "(when-not true 42)", Ok Nil )
             ]
           )
         , ( "->"
           , [ ( "(-> :ret)", Ok <| Keyword "ret" )
             , ( "(-> {:foo :bar} (assoc :buz :boo) (dissoc :foo :buz))", Ok <| Map <| ValueMap.empty )
             , ( "(-> [] seq)", Ok Nil )
             , ( "(-> {} (-> (assoc :bar :buz)) (dissoc :bar) seq)", Ok Nil )
             ]
           )
         , ( "->>"
           , [ ( "(->> :ret)", Ok <| Keyword "ret" )
             , ( "(->> [1 2 3] (map inc) (map inc) (map dec) (= (list 2 3 4)))", Ok <| Bool True )
             , ( "(->> [] seq)", Ok Nil )
             ]
           )

         -- functions
         , ( "apply"
           , [ ( "(apply + [])", Ok <| Number <| Int 0 )
             , ( "(apply + [1])", Ok <| Number <| Int 1 )
             , ( "(apply + [1 2 3])", Ok <| Number <| Int 6 )
             , ( "(apply + 1 2 [3])", Ok <| Number <| Int 6 )
             ]
           )
         , ( "assoc"
           , [ ( "(assoc nil :a 1)"
               , Ok <| Map <| ValueMap.fromList [ ( Keyword "a", Located.fakeLoc <| Number <| Int 1 ) ]
               )
             , ( "(assoc {} :a 1 :b 2)"
               , Ok <|
                    Map <|
                        ValueMap.fromList
                            [ ( Keyword "a", Located.fakeLoc <| Number <| Int 1 )
                            , ( Keyword "b", Located.fakeLoc <| Number <| Int 2 )
                            ]
               )
             , ( "(assoc {:a 1} :a 2)"
               , Ok <| Map <| ValueMap.fromList [ ( Keyword "a", Located.fakeLoc <| Number <| Int 2 ) ]
               )
             , ( "(assoc [1 2] 1 3)"
               , Ok <|
                    Vector <|
                        Array.fromList
                            [ Located { end = ( 1, 11 ), start = ( 1, 11 ) } <| Number <| Int 1
                            , Located.fakeLoc <| Number <| Int 3
                            ]
               )
             , ( "(= (assoc [] 0 1) [1])", Ok <| Bool True )
             , ( "(= (assoc [] 0 1 1 2 2 3 3 4) [1 2 3 4])", Ok <| Bool True )
             , ( "(assoc [] 1 2)", Err <| Exception "index out of bounds" )
             ]
           )
         , ( "assoc-in"
           , [ ( "(= (assoc-in nil [:foo :bar :buz] 42) {:foo {:bar {:buz 42}}})", Ok <| Bool True )
             , ( "(= (assoc-in nil [0 1 2] 42) {0 {1 {2 42}}})", Ok <| Bool True )
             , ( "(= (assoc-in [] [0 1 2] 42) [{1 {2 42}}])", Ok <| Bool True )
             , ( "(assoc-in [] [1 1 2] 42)", Err <| Exception "index out of bounds" )
             , ( "(= (assoc-in {:foo {} :bar 42} [:foo :buz] 3) {:foo {:buz 3} :bar 42})", Ok <| Bool True )
             ]
           )
         , ( "concat"
           , [ ( "(= (concat [1] [2 3] [4]) (list 1 2 3 4))", Ok <| Bool True )
             , ( "(= (concat [1 2 3] nil nil [4 5]) (list 1 2 3 4 5))", Ok <| Bool True )
             , ( "(= (concat [1] () (list 2 3) {4 5}) (list 1 2 3 [4 5]))", Ok <| Bool True )
             ]
           )
         , ( "conj"
           , [ ( "(= (conj [1 2] 3) [1 2 3])", Ok <| Bool True )
             , ( "(= (conj [] 1) [1])", Ok <| Bool True )
             , ( "(= (conj [] 1 2 3) [1 2 3])", Ok <| Bool True )
             , ( "(= (conj nil 1) (list 1))", Ok <| Bool True )
             , ( "(= (conj (list 2 3) 1) (list 1 2 3))", Ok <| Bool True )
             , ( "(= (conj (list) 1 2 3) (list 3 2 1))", Ok <| Bool True )
             , ( "(= (conj #{} 1 2 3) #{1 2 3})", Ok <| Bool True )
             , ( "(= (conj {} [1 2]) {1 2})", Ok <| Bool True )
             , ( "(= (conj {} (first {3 4})) {3 4})", Ok <| Bool True )
             ]
           )
         , ( "cons"
           , [ ( "(= (cons 3 [1 2]) (list 3 1 2))", Ok <| Bool True )
             , ( "(= (cons 1 []) (list 1))", Ok <| Bool True )
             , ( "(= (cons 1 nil) (list 1))", Ok <| Bool True )
             , ( "(= (cons 1 (list 2 3)) (list 1 2 3))", Ok <| Bool True )
             , ( "(= (cons 1 #{}) (list 1))", Ok <| Bool True )
             , ( "(= (cons [1 2] {}) (list [1 2]))", Ok <| Bool True )
             , ( "(= (cons (first {3 4}) {}) (list [3 4]))", Ok <| Bool True )
             ]
           )
         , ( "contains?"
           , [ ( "(contains? {:a 3} :a)", Ok <| Bool True )
             , ( "(contains? {:a 3} :b)", Ok <| Bool False )
             , ( "(contains? #{42} 42)", Ok <| Bool True )
             , ( "(contains? #{42} 43)", Ok <| Bool False )
             , ( "(contains? nil :anything)", Ok <| Bool False )
             , ( "(contains? [3 4] 1)", Ok <| Bool True )
             , ( "(contains? [3 4] 2)", Ok <| Bool False )
             , ( "(contains? nil 2)", Ok <| Bool False )
             ]
           )
         , ( "complement"
           , [ ( "((complement neg?) (- 3))", Ok <| Bool False )
             , ( "((complement seq) [])", Ok <| Bool True )
             ]
           )
         , ( "dec"
           , [ ( "(dec 1)", Ok <| Number <| Int 0 )
             , ( "(dec (- 1))", Ok <| Number <| Int -2 )
             ]
           )
         , ( "drop"
           , [ ( "(drop 0 [])", Ok <| List [] )
             , ( "(drop 0 nil)", Ok <| List [] )
             , ( "(drop 100 nil)", Ok <| List [] )
             , ( "(= (drop 1 [1 2 3]) (list 2 3))", Ok <| Bool True )
             , ( "(= (drop 2 [1 2 3]) (list 3))", Ok <| Bool True )
             , ( "(= (drop 3 [1 2 3]) (list))", Ok <| Bool True )
             , ( "(= (drop 30 [1 2 3]) (list))", Ok <| Bool True )
             ]
           )
         , ( "drop-while"
           , [ ( "(drop-while odd? [])", Ok <| List [] )
             , ( "(drop-while odd? nil)", Ok <| List [] )
             , ( "(drop-while even? nil)", Ok <| List [] )
             , ( "(= (drop-while odd? [1 3 5 6 8]) (list 6 8))", Ok <| Bool True )
             , ( "(= (drop-while odd? [1 2 3]) (list 2 3))", Ok <| Bool True )
             , ( "(= (drop-while odd? [2 3]) (list 2 3))", Ok <| Bool True )
             , ( "(= (drop-while even? [1 2 3]) (list 1 2 3))", Ok <| Bool True )
             ]
           )
         , ( "even?"
           , [ ( "(even? 2)", Ok <| Bool True )
             , ( "(even? 3)", Ok <| Bool False )
             , ( "(even? 2.0)", Err <| Exception "Argument must be an integer: 2" )
             ]
           )
         , ( "every?"
           , [ ( "(every? odd? nil)", Ok <| Bool True )
             , ( "(every? odd? [])", Ok <| Bool True )
             , ( "(every? (comp integer? key) {1 2 3 4 5 6})", Ok <| Bool True )
             , ( "(every? odd? [1 3 5])", Ok <| Bool True )
             , ( "(every? odd? [1 3 6])", Ok <| Bool False )
             ]
           )
         , ( "filter"
           , [ ( "(filter pos? nil)", Ok <| List [] )
             , ( "(= (filter odd? [1 2 3 4 5]) (list 1 3 5))", Ok <| Bool True )
             ]
           )
         , ( "identity"
           , [ ( "(identity :ret)", Ok <| Keyword "ret" ) ]
           )
         , ( "inc"
           , [ ( "(inc 0)", Ok <| Number <| Int 1 )
             , ( "(inc (- 1))", Ok <| Number <| Int 0 )
             ]
           )
         , ( "into"
           , [ ( "(= (into [] (list 1 2 3 4)) [1 2 3 4])", Ok <| Bool True )
             , ( "(= (into () (list 1 2 3 4)) (list 4 3 2 1))", Ok <| Bool True )
             , ( "(= (into {} (list [1 2] [3 4])) {1 2 3 4})", Ok <| Bool True )
             , ( "(= (into #{} (list 1 2 3 4)) #{1 2 3 4})", Ok <| Bool True )
             ]
           )
         , ( "comp"
           , [ ( "((comp) :ret)", Ok <| Keyword "ret" )
             , ( "((comp inc inc dec) 3)", Ok <| Number <| Int 4 )
             , ( "((comp #(* 2 %) inc #(* 3 %)) 3)", Ok <| Number <| Int 20 )
             ]
           )
         , ( "dissoc"
           , [ ( "(dissoc {:a 42} :a)", Ok <| Map ValueMap.empty )
             , ( "(dissoc {:a nil} :b)"
               , Ok <|
                    Map <|
                        ValueMap.fromList [ ( Keyword "a", Located { end = ( 1, 16 ), start = ( 1, 16 ) } Nil ) ]
               )
             , ( "(dissoc nil :a)", Ok Nil )
             ]
           )
         , ( "first"
           , [ ( "(first [])", Ok Nil )
             , ( "(first [1 2])", Ok <| Number <| Int 1 )
             , ( "(first nil)", Ok Nil )
             , ( "(first {})", Ok Nil )
             , ( "(first {1 2})"
               , Ok <|
                    MapEntry ( Number <| Int 1, Located { end = ( 1, 12 ), start = ( 1, 12 ) } <| Number <| Int 2 )
               )
             , ( "(first #{})", Ok Nil )
             , ( "(first #{2})", Ok <| Number <| Int 2 )
             , ( "(first (list))", Ok Nil )
             , ( "(first (list 1 2 3))", Ok <| Number <| Int 1 )
             ]
           )
         , ( "float?"
           , [ ( "(float? 42.0)", Ok <| Bool True )
             , ( "(float? 42)", Ok <| Bool False )
             , ( "(float? :float)", Ok <| Bool False )
             ]
           )
         , ( "get"
           , [ ( "(get nil :something)", Ok <| Nil )
             , ( "(get nil :something :anything)", Ok <| Keyword "anything" )
             , ( "(get {:a 42} :a)", Ok <| Number <| Int 42 )
             , ( "(get {:a 42} :b)", Ok Nil )
             , ( "(get {} :b :default)", Ok <| Keyword "default" )
             , ( "(get {:b nil} :b :default)", Ok Nil )
             , ( "(get [] 0)", Ok Nil )
             , ( "(get [] 0 :default)", Ok <| Keyword "default" )
             , ( "(get [1] 0 :default)", Ok <| Number <| Int 1 )
             , ( "(get #{} 0)", Ok Nil )
             , ( "(get #{} 0 :default)", Ok <| Keyword "default" )
             , ( "(get #{0} 0 :default)", Ok <| Number <| Int 0 )
             ]
           )
         , ( "get-in"
           , [ ( "(get-in nil [:a 0 :b])", Ok Nil )
             , ( "(get-in nil [:a])", Ok Nil )
             , ( "(get-in nil [])", Ok Nil )
             , ( "(get-in {} [:a 0 :b])", Ok Nil )
             , ( "(get-in {} [:a])", Ok Nil )
             , ( "(get-in {} [])", Ok <| Map <| ValueMap.empty )
             , ( "(get-in [3 [{:foo 2}]] [1 0 :foo])", Ok <| Number <| Int 2 )
             , ( "(get-in [3 [{:foo 2}]] [0])", Ok <| Number <| Int 3 )
             , ( "(get-in [] [4])", Ok Nil )
             , ( "(get-in [] [:foo])", Ok Nil )
             , ( "(get-in {:foo {:bar {:buz [0 1 2 3]}}} [:foo :bar :buz 3])", Ok <| Number <| Int 3 )
             ]
           )
         , ( "json/encode"
           , [ ( "(json/encode {})", Ok <| String "{}" )
             , ( "(json/encode {\"key\" \"value\"})", Ok <| String "{\"key\":\"value\"}" )
             , ( "(json/encode [])", Ok <| String "[]" )
             , ( "(json/encode [{} {}])", Ok <| String "[{},{}]" )
             , ( "(json/encode ())", Ok <| String "[]" )
             , ( "(json/encode 42)", Ok <| String "42" )
             , ( "(json/encode 42.5)", Ok <| String "42.5" )
             , ( "(json/encode \"\")", Ok <| String "\"\"" )
             , ( "(json/encode :kw)", Ok <| String "\"kw\"" )
             , ( "(json/encode nil)", Ok <| String "null" )
             , ( "(json/encode true)", Ok <| String "true" )
             , ( "(json/encode false)", Ok <| String "false" )
             , ( "(json/encode (first {\"key\" \"value\"}))", Ok <| String "[\"key\",\"value\"]" )
             , ( "(json/encode #{})", Ok <| String "[]" )
             , ( "(json/encode #{1})", Ok <| String "[1]" )
             ]
           )
         , ( "json/decode"
           , [ ( "(json/decode \"[]\")", Ok <| Vector <| Array.empty )
             , ( "(json/decode \"{}\")", Ok <| Map <| ValueMap.empty )
             , ( "(json/decode \"\\\"foo\\\"\")", Ok <| String "foo" )
             , ( "(json/decode \"null\")", Ok <| Nil )
             , ( "(json/decode \"true\")", Ok <| Bool True )
             , ( "(json/decode \"false\")", Ok <| Bool False )
             , ( "(json/decode \"42\")", Ok <| Number <| Int 42 )
             , ( "(json/decode \"42.5\")", Ok <| Number <| Float 42.5 )
             ]
           )
         , ( "integer?"
           , [ ( "(integer? 42)", Ok <| Bool True )
             , ( "(integer? 42.5)", Ok <| Bool False )
             , ( "(integer? \"42\")", Ok <| Bool False )
             , ( "(integer? nil)", Ok <| Bool False )
             ]
           )
         , ( "key"
           , [ ( "(key (first {1 2}))", Ok <| Number <| Int 1 ) ]
           )
         , ( "last"
           , [ ( "(last nil)", Ok Nil )
             , ( "(last [])", Ok Nil )
             , ( "(last #{})", Ok Nil )
             , ( "(last ())", Ok Nil )
             , ( "(last [:foo :bar :ret])", Ok <| Keyword "ret" )
             , ( "(last (list :foo :bar :ret))", Ok <| Keyword "ret" )
             ]
           )
         , ( "list"
           , [ ( "(list nil nil)", Ok <| List [ Located.fakeLoc Nil, Located.fakeLoc Nil ] )
             , ( "(list)", Ok <| List [] )
             ]
           )
         , ( "mod"
           , [ ( "(mod 5 2)", Ok <| Number <| Int 1 )
             , ( "(mod 7 (- 2))", Ok <| Number <| Int -1 )
             , ( "(mod (- 9) (- 2))", Ok <| Number <| Int -1 )
             ]
           )
         , ( "Exception."
           , [ ( "(Exception. \"error\")", Ok <| Throwable <| Exception "error" ) ]
           )
         , ( "map"
           , [ ( "(map inc nil)", Ok <| List [] )
             , ( "(= (map inc [1 2 3]) (list 2 3 4))", Ok <| Bool True )
             , ( "(= (map #(key %) {1 2}) (list 1))", Ok <| Bool True )
             ]
           )
         , ( "mapcat"
           , [ ( "(= (mapcat (fn [i] [i i]) [1 2 3]) (list 1 1 2 2 3 3))", Ok <| Bool True )
             , ( "(= (mapcat identity [[1 2] nil [3 4]]) (list 1 2 3 4))", Ok <| Bool True )
             ]
           )
         , ( "map-indexed"
           , [ ( "(= (map-indexed (fn [i e] [i e]) [1 2 3]) (list [0 1] [1 2] [2 3]))", Ok <| Bool True ) ]
           )
         , ( "neg?"
           , [ ( "(neg? 3)", Ok <| Bool False )
             , ( "(neg? 0)", Ok <| Bool False )
             , ( "(neg? (- 10))", Ok <| Bool True )
             ]
           )
         , ( "next"
           , [ ( "(next nil)", Ok Nil )
             , ( "(next #{})", Ok Nil )
             , ( "(next ())", Ok Nil )
             , ( "(next [])", Ok Nil )
             , ( "(next {})", Ok Nil )
             , ( "(next [1])", Ok Nil )
             , ( "(next #{1}))", Ok Nil )
             , ( "(next (list 1))", Ok Nil )
             , ( "(= (next [1 2 3]) (list 2 3))", Ok <| Bool True )
             , ( "(= (next (list 1 2 3)) (list 2 3))", Ok <| Bool True )
             ]
           )
         , ( "not"
           , [ ( "(not true)", Ok <| Bool False )
             , ( "(not false)", Ok <| Bool True )
             , ( "(not nil)", Ok <| Bool True )
             , ( "(not 42)", Ok <| Bool False )
             ]
           )
         , ( "number?"
           , [ ( "(number? 42)", Ok <| Bool True )
             , ( "(number? 42.5)", Ok <| Bool True )
             , ( "(number? \"42.5\")", Ok <| Bool False )
             , ( "(number? nil)", Ok <| Bool False )
             ]
           )
         , ( "odd?"
           , [ ( "(odd? 2)", Ok <| Bool False )
             , ( "(odd? 3)", Ok <| Bool True )
             , ( "(odd? 2.0)", Err <| Exception "Argument must be an integer: 2" )
             ]
           )
         , ( "peek"
           , [ ( "(peek (list 1 2 3))", Ok <| Number <| Int 1 )
             , ( "(peek [1 2 3])", Ok <| Number <| Int 3 )
             , ( "(peek nil)", Ok Nil )
             ]
           )
         , ( "pos?"
           , [ ( "(pos? 3)", Ok <| Bool True )
             , ( "(pos? 0)", Ok <| Bool False )
             , ( "(pos? (- 10))", Ok <| Bool False )
             ]
           )
         , ( "pr-str"
           , [ ( "(pr-str 42)", Ok <| String "42" )
             , ( "(pr-str 42.5)", Ok <| String "42.5" )
             , ( "(pr-str pr-str)", Ok <| String "fn<pr-str>" )
             , ( "(pr-str ())", Ok <| String "()" )
             , ( "(pr-str [])", Ok <| String "[]" )
             , ( "(pr-str nil)", Ok <| String "nil" )
             , ( "(pr-str false)", Ok <| String "false" )
             , ( "(pr-str true)", Ok <| String "true" )
             , ( "(pr-str :keyword)", Ok <| String ":keyword" )
             , ( "(pr-str {})", Ok <| String "{}" )
             , ( "(pr-str (first {1 2}))", Ok <| String "[1 2]" )
             , ( "(pr-str #{})", Ok <| String "#{}" )
             ]
           )
         , ( "reduce"
           , [ ( "(reduce + [])", Ok <| Number <| Int 0 )
             , ( "(reduce + 2 [])", Ok <| Number <| Int 2 )
             , ( "(reduce + [1 2 3 4 5])", Ok <| Number <| Int 15 )
             , ( "(reduce + 3 [1 2 3 4 5])", Ok <| Number <| Int 18 )
             ]
           )
         , ( "reduce-kv"
           , [ ( "(= (reduce-kv (fn [a k v] (conj a k v)) #{} {1 2 3 4 5 6}) #{1 2 3 4 5 6})", Ok <| Bool True ) ]
           )
         , ( "rem"
           , [ ( "(rem 8 3)", Ok <| Number <| Int 2 )
             , ( "(rem 8 2)", Ok <| Number <| Int 0 )
             , ( "(rem 8.0 3.0)", Ok <| Number <| Float 2.0 )
             , ( "(rem 8.0 2.0)", Ok <| Number <| Float 0.0 )
             , ( "(rem 8.0 3)", Ok <| Number <| Float 2.0 )
             , ( "(rem 8 3.0)", Ok <| Number <| Float 2.0 )
             ]
           )
         , ( "remove"
           , [ ( "(remove neg? nil)", Ok <| List [] )
             , ( "(= (remove even? [1 2 3 4 5]) (list 1 3 5))", Ok <| Bool True )
             ]
           )
         , ( "repeat"
           , [ ( "(= (repeat 0 nil) ())", Ok <| Bool True )
             , ( "(= (repeat (- 1) nil) ())", Ok <| Bool True )
             , ( "(= (repeat 1 nil) (list nil))", Ok <| Bool True )
             , ( "(= (repeat 5 1) (list 1 1 1 1 1))", Ok <| Bool True )
             ]
           )
         , ( "rest"
           , [ ( "(= (rest (list 1 2 3)) (list 2 3))", Ok <| Bool True )
             , ( "(= (rest [1 2 3]) (list 2 3))", Ok <| Bool True )
             , ( "(= (rest nil) (list))", Ok <| Bool True )
             ]
           )
         , ( "reverse"
           , [ ( "(= (reverse [1 2 3]) (list 3 2 1))", Ok <| Bool True )
             , ( "(= (reverse [4 3 2]) (list 2 3 4))", Ok <| Bool True )
             , ( "(reverse nil)", Ok <| List [] )
             ]
           )
         , ( "second"
           , [ ( "(second nil)", Ok Nil )
             , ( "(second (list 1 2 3))", Ok <| Number <| Int 2 )
             , ( "(second [1 2 3])", Ok <| Number <| Int 2 )
             , ( "(second ())", Ok Nil )
             , ( "(second [1])", Ok Nil )
             , ( "(second #{})", Ok Nil )
             , ( "(second {})", Ok Nil )
             , ( "(second (first {1 2}))", Ok <| Number <| Int 2 )
             ]
           )
         , ( "seq"
           , [ ( "(seq nil)", Ok Nil )
             , ( "(seq [])", Ok Nil )
             , ( "(seq ())", Ok Nil )
             , ( "(seq {})", Ok Nil )
             , ( "(seq #{})", Ok Nil )
             , ( "(= (seq [1]) (list 1))", Ok <| Bool True )
             , ( "(= (seq (list 1)) (list 1))", Ok <| Bool True )
             , ( "(= (seq {1 2}) (list [1 2]))", Ok <| Bool True )
             , ( "(= (seq #{1}) (list 1))", Ok <| Bool True )
             ]
           )
         , ( "str"
           , [ ( "(str)", Ok <| String "" )
             , ( "(str 42)", Ok <| String "42" )
             , ( "(str 42.3)", Ok <| String "42.3" )
             , ( "(str \"1\")", Ok <| String "1" )
             , ( "(str \"1\" \"2\" \"3\" \"4\" \"5\")", Ok <| String "12345" )
             , ( "(str str)", Ok <| String "fn<str>" )
             , ( "(str ())", Ok <| String "()" )
             , ( "(str [])", Ok <| String "[]" )
             , ( "(str nil)", Ok <| String "" )
             , ( "(str false)", Ok <| String "false" )
             , ( "(str true)", Ok <| String "true" )
             , ( "(str :keyword)", Ok <| String ":keyword" )
             , ( "(str {})", Ok <| String "{}" )
             , ( "(str (first {1 2}))", Ok <| String "[1 2]" )
             , ( "(str #{})", Ok <| String "#{}" )
             ]
           )
         , ( "string/join"
           , [ ( "(string/join nil)", Ok <| String "" )
             , ( "(string/join [\"1\" \"2\" \"3\"])", Ok <| String "123" )
             , ( "(string/join [{} {} {}])", Ok <| String "{}{}{}" )
             , ( "(string/join \", \" [\"1\" \"2\" \"3\"])", Ok <| String "1, 2, 3" )
             , ( "(string/join \", \" [1 2 3])", Ok <| String "1, 2, 3" )
             , ( "(string/join \", \" nil)", Ok <| String "" )
             ]
           )
         , ( "string/length"
           , [ ( "(string/length \"butterfly\")", Ok <| Number <| Int <| 9 )
             , ( "(string/length \"\")", Ok <| Number <| Int <| 0 )
             ]
           )
         , ( "string/split-lines"
           , [ ( "(= (string/split-lines \"a\\nb\\nc\") (list \"a\" \"b\" \"c\"))", Ok <| Bool True )
             , ( "(= (string/split-lines \"a\\n\\nb\") (list \"a\" \"\" \"b\"))", Ok <| Bool True )
             , ( "(= (string/split-lines \"\") (list \"\"))", Ok <| Bool True )
             ]
           )
         , ( "take"
           , [ ( "(take 0 [])", Ok <| List [] )
             , ( "(take 0 nil)", Ok <| List [] )
             , ( "(take 100 nil)", Ok <| List [] )
             , ( "(= (take 1 [1 2 3]) (list 1))", Ok <| Bool True )
             , ( "(= (take 2 [1 2 3]) (list 1 2))", Ok <| Bool True )
             , ( "(= (take 3 [1 2 3]) (list 1 2 3))", Ok <| Bool True )
             , ( "(= (take 30 [1 2 3]) (list 1 2 3))", Ok <| Bool True )
             ]
           )
         , ( "take-while"
           , [ ( "(take-while odd? [])", Ok <| List [] )
             , ( "(take-while odd? nil)", Ok <| List [] )
             , ( "(take-while even? nil)", Ok <| List [] )
             , ( "(= (take-while odd? [1 3 5 6 8]) (list 1 3 5))", Ok <| Bool True )
             , ( "(= (take-while odd? [1 2 3]) (list 1))", Ok <| Bool True )
             , ( "(= (take-while odd? [2 3]) ())", Ok <| Bool True )
             , ( "(= (take-while even? [1 2 3]) ())", Ok <| Bool True )
             ]
           )
         , ( "throw"
           , [ ( "(throw (Exception. \"hi\"))", Err <| Exception "hi" )
             , ( "(throw nil)", Err <| Exception "nil is not throwable" )
             ]
           )
         , ( "val"
           , [ ( "(val (first {1 2}))", Ok <| Number <| Int 2 )
             ]
           )
         , ( "zero?"
           , [ ( "(zero? 3)", Ok <| Bool False )
             , ( "(zero? 0)", Ok <| Bool True )
             , ( "(zero? (- 10))", Ok <| Bool False )
             ]
           )
         ]
            |> List.map
                (\( section, examples ) ->
                    describe section
                        (List.map
                            (\( code, expect ) ->
                                test code <|
                                    \_ ->
                                        eval code
                                            |> Expect.equal expect
                            )
                            examples
                        )
                )
        )
