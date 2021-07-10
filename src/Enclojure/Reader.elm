module Enclojure.Reader exposing (parse)

import Enclojure.Located exposing (Located(..))
import Enclojure.Reader.Macros as Macros
import Enclojure.Runtime exposing (Exception(..), Value(..))
import Parser exposing ((|.), (|=), Parser)
import Set


located : Parser a -> Parser (Located a)
located p =
    Parser.succeed (\v start end -> Located { start = start, end = end } v)
        |= p
        |= Parser.getPosition
        |= Parser.getPosition


parse : String -> Result (List Parser.DeadEnd) (List (Located Value))
parse code =
    Parser.run parser code


int : Parser Value
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


symbol : Parser Value
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


expressionsHelper : List (Located Value) -> Parser (Parser.Step (List (Located Value)) (List (Located Value)))
expressionsHelper revExprs =
    Parser.oneOf
        [ Parser.succeed (\expr -> Parser.Loop (expr :: revExprs))
            |= located expression
            |. spaces
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revExprs))
        ]


expression : Parser Value
expression =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ list
            , vector
            , bool
            , nil
            , symbol
            , int
            ]
        |. Parser.spaces


spaces : Parser ()
spaces =
    Parser.oneOf
        [ Parser.spaces
        , Parser.chompIf (\c -> c == ',' || c == ';')
        ]


nil : Parser Value
nil =
    Parser.keyword "nil" |> Parser.map (always Nil)


true : Parser Value
true =
    Parser.keyword "true" |> Parser.map (always (Bool True))


bool : Parser Value
bool =
    Parser.oneOf [ true, false ]


false : Parser Value
false =
    Parser.keyword "false" |> Parser.map (always (Bool False))


vector : Parser Value
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


list : Parser Value
list =
    Parser.sequence
        { start = "("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> Parser.map List


parser : Parser (List (Located Value))
parser =
    Parser.loop [] expressionsHelper
        |> Parser.andThen
            (\l ->
                List.foldl (\e a -> a |> Result.andThen (\lr -> Macros.macroexpandAll e |> Result.map (\v -> v :: lr)))
                    (Ok [])
                    l
                    |> (\r ->
                            case r of
                                Ok v ->
                                    Parser.succeed v

                                Err (Exception e) ->
                                    Parser.problem e
                       )
            )
