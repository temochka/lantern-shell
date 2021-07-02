module Enclojure.Parser exposing (Expr(..), Number, parse)

import Enclojure.Located as Located exposing (Located(..))
import Parser exposing ((|.), (|=), Parser)
import Set


type alias Number =
    Int


type Expr
    = List (List (Located Expr))
    | Apply (Located Expr) (Located Expr)
      -- | Quote Expr
    | Symbol String
    | Number Number
    | Nil
    | Bool Basics.Bool
    | Do (List (Located Expr))
    | Conditional { ifExpression : Located Expr, thenExpression : Located Expr, elseExpression : Maybe (Located Expr) }
    | Def String (Located Expr)
    | Vector (List (Located Expr))


located : Parser a -> Parser (Located a)
located p =
    Parser.succeed (\v start end -> Located { start = start, end = end } v)
        |= p
        |= Parser.getPosition
        |= Parser.getPosition


parse : String -> Result (List Parser.DeadEnd) (Located Expr)
parse code =
    Parser.run parser code


int : Parser Expr
int =
    Parser.oneOf
        [ Parser.map Number <|
            Parser.succeed negate
                |. Parser.symbol "-"
                |= Parser.int
        , Parser.succeed Number |= Parser.int
        ]


isAllowedSymbolSpecialChar : Char -> Bool
isAllowedSymbolSpecialChar c =
    c
        == '-'
        || c
        == '+'
        || c
        == '/'
        || c
        == '-'
        || c
        == '*'
        || c
        == '>'
        || c
        == '<'
        || c
        == '='
        || c
        == '\''
        || c
        == '&'


symbol : Parser Expr
symbol =
    Parser.succeed Symbol
        |= Parser.variable
            { start =
                \c ->
                    Char.isAlpha c
                        || isAllowedSymbolSpecialChar c
            , inner = \c -> Char.isAlphaNum c || isAllowedSymbolSpecialChar c
            , reserved = Set.empty
            }


expressionsHelper : List (Located Expr) -> Parser (Parser.Step (List (Located Expr)) (List (Located Expr)))
expressionsHelper revExprs =
    Parser.oneOf
        [ Parser.succeed (\expr -> Parser.Loop (expr :: revExprs))
            |= located expression
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revExprs))
        ]


rootLevelDo : Parser Expr
rootLevelDo =
    Parser.loop [] expressionsHelper
        |> Parser.map Do


expression : Parser Expr
expression =
    Parser.oneOf
        [ list
        , vector
        , bool
        , nil
        , symbol
        , int
        ]


spaces : Parser ()
spaces =
    Parser.oneOf
        [ Parser.spaces
        , Parser.chompIf (\c -> c == ',' || c == ';')
        ]


nil : Parser Expr
nil =
    Parser.keyword "nil" |> Parser.map (always Nil)


true : Parser Expr
true =
    Parser.keyword "true" |> Parser.map (always (Bool True))


bool : Parser Expr
bool =
    Parser.oneOf [ true, false ]


false : Parser Expr
false =
    Parser.keyword "false" |> Parser.map (always (Bool False))


vector : Parser Expr
vector =
    Parser.sequence
        { start = "["
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = "]"
        }
        |> Parser.map Vector


list : Parser Expr
list =
    Parser.sequence
        { start = "("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> Parser.andThen
            (\l ->
                case l of
                    (Located _ (Symbol "do")) :: rest ->
                        Parser.succeed (Do rest)

                    (Located _ (Symbol "def")) :: args ->
                        case args of
                            _ :: _ :: _ :: _ ->
                                Parser.problem "too many arguments to def"

                            [ Located _ (Symbol name), expr ] ->
                                Parser.succeed (Def name expr)

                            [ _, _ ] ->
                                Parser.problem "def accepts a symbol and an expression"

                            [ Located _ (Symbol name) ] ->
                                Parser.problem ("empty def " ++ name)

                            [ _ ] ->
                                Parser.problem "foo"

                            [] ->
                                Parser.problem "no arguments to def"

                    (Located _ (Symbol "if")) :: rest ->
                        case rest of
                            _ :: _ :: _ :: _ :: _ ->
                                Parser.problem "an if with too many forms"

                            [ eIf, eThen, eElse ] ->
                                Parser.succeed (Conditional { ifExpression = eIf, thenExpression = eThen, elseExpression = Just eElse })

                            [ eIf, eThen ] ->
                                Parser.succeed (Conditional { ifExpression = eIf, thenExpression = eThen, elseExpression = Nothing })

                            [ _ ] ->
                                Parser.problem "an if without then"

                            [] ->
                                Parser.problem "an empty if"

                    expr :: loc :: rest ->
                        Apply expr (Located.replace loc (List (loc :: rest))) |> Parser.succeed

                    [ expr ] ->
                        Apply expr (Located.replace expr (List [])) |> Parser.succeed

                    [] ->
                        List [] |> Parser.succeed
            )


parser : Parser (Located Expr)
parser =
    located rootLevelDo
