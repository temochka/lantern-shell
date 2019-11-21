module DevTools.ArgumentParser exposing (parse)

import Dict
import Parser exposing ((|.), (|=), Parser)
import Set


parse : String -> List String
parse sql =
    sql
        |> Parser.run (Parser.loop [] parserHelper)
        |> Result.withDefault []


parserHelper : List String -> Parser (Parser.Step (List String) (List String))
parserHelper identifiers =
    Parser.oneOf
        [ identifier
            |> Parser.map (\id -> Parser.Loop (id :: identifiers))
        , singleQuotedString
            |> Parser.map (\id -> Parser.Loop identifiers)
        , Parser.chompWhile (\c -> not <| isInterestingChar c)
            |> Parser.getChompedString
            |> Parser.map
                (\s ->
                    if String.isEmpty s then
                        Parser.Done identifiers

                    else
                        Parser.Loop identifiers
                )
        ]


identifier : Parser String
identifier =
    Parser.variable
        { start = isIdentifierStart
        , inner = \c -> Char.isAlphaNum c
        , reserved = Set.empty
        }


isInterestingChar : Char -> Bool
isInterestingChar c =
    isIdentifierStart c || c == '\'' || c == '"'



-- "


isIdentifierStart : Char -> Bool
isIdentifierStart c =
    c == '$' || c == '@' || c == ':'


singleQuotedString : Parser ()
singleQuotedString =
    let
        helper _ =
            Parser.oneOf
                [ Parser.symbol "'" |> Parser.map (always (Parser.Done ()))
                , Parser.chompWhile (\c -> c /= '\'')
                    |> Parser.getChompedString
                    |> Parser.map
                        (\s ->
                            if String.isEmpty s then
                                Parser.Done ()

                            else
                                Parser.Loop ()
                        )
                ]
    in
    Parser.succeed ()
        |. Parser.symbol "'"
        |. Parser.loop () helper


doubleQuotedString : Parser ()
doubleQuotedString =
    let
        helper _ =
            Parser.oneOf
                [ Parser.symbol "\"" |> Parser.map (always (Parser.Done ()))
                , Parser.chompWhile (\c -> c /= '"')
                    -- "
                    |> Parser.getChompedString
                    |> Parser.map
                        (\s ->
                            if String.isEmpty s then
                                Parser.Done ()

                            else
                                Parser.Loop ()
                        )
                ]
    in
    Parser.succeed ()
        |. Parser.symbol "\""
        |. Parser.loop () helper
