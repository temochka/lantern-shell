module Enclojure.Parser exposing (Expr(..), Number, parse)

import Enclojure.Located exposing (Located)
import Parser exposing ((|.), (|=), Parser)
import Set


type alias Number =
    Int


type Expr
    = List (List (Located Expr))
      -- | Quote Expr
    | Symbol String
    | Number Number



-- | Do (List Expr)


located : Parser a -> Parser (Located a)
located p =
    Parser.succeed Located
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


expression : Parser Expr
expression =
    Parser.oneOf
        [ symbol
        , list
        , int
        ]


list : Parser Expr
list =
    Parser.succeed List
        |= Parser.sequence
            { start = "("
            , separator = ""
            , spaces = Parser.spaces
            , item = Parser.lazy (\_ -> located expression)
            , trailing = Parser.Optional
            , end = ")"
            }


parser : Parser (Located Expr)
parser =
    located expression
