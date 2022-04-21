module Enclojure.Reader exposing (parse)

import Array
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Reader.DoubleQuotedString as DoubleQuotedString
import Enclojure.Reader.Macros as Macros
import Enclojure.Types exposing (..)
import Enclojure.ValueMap as ValueMap
import Enclojure.ValueSet as ValueSet
import Parser exposing ((|.), (|=), Parser)
import Set


located : Parser a -> Parser (Located a)
located p =
    Parser.succeed (\start v end -> Located.at start end v)
        |= Parser.getPosition
        |= p
        |= Parser.getPosition


parse : String -> Result (List Parser.DeadEnd) (List (Located Value))
parse code =
    Parser.run parser code


number : Parser Value
number =
    Parser.number
        { int = Just (Int >> Number)
        , float = Just (Float >> Number)
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        }
        |> Parser.backtrackable


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
        || c
        == '?'
        || c
        == '.'
        || c
        == '$'
        || c
        == '_'


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
        , Parser.succeed (\_ -> Parser.Loop revExprs)
            |= lineComment
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse revExprs))
        ]


lineComment : Parser ()
lineComment =
    Parser.lineComment ";"


expression : Parser Value
expression =
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.oneOf
            [ lambda
            , string
            , list
            , vector
            , valueMap
            , valueSet
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
        , Parser.chompIf (\c -> c == ',')
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
        |> Parser.map (Array.fromList >> Vector)


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


mapEntry : Parser ValueMapEntry
mapEntry =
    Parser.succeed Tuple.pair
        |= expression
        |. spaces
        |= located expression


valueMap : Parser Value
valueMap =
    Parser.sequence
        { start = "{"
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> mapEntry)
        , trailing = Parser.Optional
        , end = "}"
        }
        |> Parser.map (ValueMap.fromList >> Map)


valueSet : Parser Value
valueSet =
    Parser.sequence
        { start = "#{"
        , separator = ""
        , spaces = spaces
        , item = Parser.lazy (\_ -> expression)
        , trailing = Parser.Optional
        , end = "}"
        }
        |> Parser.map (ValueSet.fromList >> Set)


string : Parser Value
string =
    DoubleQuotedString.string |> Parser.map String


parser : Parser (List (Located Value))
parser =
    Parser.loop [] expressionsHelper
        |> Parser.andThen
            (\l ->
                List.foldr (\e a -> a |> Result.andThen (\lr -> Macros.macroexpandAll e |> Result.map (\v -> v :: lr)))
                    (Ok [])
                    l
                    |> (\r ->
                            case r of
                                Ok v ->
                                    Parser.succeed v

                                Err (Exception e _) ->
                                    Parser.problem e
                       )
            )
