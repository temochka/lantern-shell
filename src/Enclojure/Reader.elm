module Enclojure.Reader exposing (parse)

import Enclojure.HashMap as HashMap
import Enclojure.Located exposing (Located(..))
import Enclojure.Reader.DoubleQuotedString as DoubleQuotedString
import Enclojure.Reader.Macros as Macros
import Enclojure.Types exposing (..)
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


number : Parser Value
number =
    Parser.number
        { int = Just Int
        , float = Just Float
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        }


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
        || c
        == '%'


symbolLike : Parser String
symbolLike =
    Parser.variable
        { start =
            \c ->
                Char.isAlpha c
                    || isAllowedSymbolSpecialChar c
        , inner = \c -> Char.isAlphaNum c || isAllowedSymbolSpecialChar c
        , reserved = Set.empty
        }


symbol : Parser Value
symbol =
    Parser.succeed Symbol
        |= symbolLike


keyword : Parser Value
keyword =
    Parser.succeed Keyword
        |. Parser.symbol ":"
        |= symbolLike


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
            [ lambda
            , string
            , list
            , vector
            , hashMap
            , bool
            , nil
            , number
            , symbol
            , keyword
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


lambda : Parser Value
lambda =
    Parser.sequence
        { start = "#("
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> located expression)
        , trailing = Parser.Optional
        , end = ")"
        }
        |> located
        |> Parser.map (\(Located loc v) -> List (Located loc (Symbol "__lambda") :: v))


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


mapEntry : Parser HashMapEntry
mapEntry =
    Parser.succeed Tuple.pair
        |= expression
        |. spaces
        |= located expression


hashMap : Parser Value
hashMap =
    Parser.sequence
        { start = "{"
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> mapEntry)
        , trailing = Parser.Optional
        , end = "}"
        }
        |> Parser.map (HashMap.fromList >> Map)


string : Parser Value
string =
    DoubleQuotedString.string |> Parser.map String


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
