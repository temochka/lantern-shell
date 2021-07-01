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
    | Do (List (Located Expr))



-- | Do (List Expr)


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



-- rootLevelDo : Parser Expr
-- rootLevelDo =
--     Parser.loop [] expressionsHelper
--         |> Parser.map Do


expression : Parser Expr
expression =
    Parser.oneOf
        [ listForm
        , symbol
        , int
        ]


spaces : Parser ()
spaces =
    Parser.oneOf
        [ Parser.spaces
        , Parser.chompIf (\c -> c == ',' || c == ';')
        ]


listForm : Parser Expr
listForm =
    Parser.sequence
        { start = "("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> Parser.map
            (\list ->
                case list of
                    (Located _ (Symbol "do")) :: rest ->
                        Do rest

                    expr :: loc :: rest ->
                        Apply expr (Located.replace loc (List (loc :: rest)))

                    [ expr ] ->
                        Apply expr (Located.replace expr (List []))

                    [] ->
                        List []
            )


parser : Parser (Located Expr)
parser =
    -- located rootLevelDo
    located expression
