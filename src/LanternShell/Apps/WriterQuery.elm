module LanternShell.Apps.WriterQuery exposing (Message, Model, init, lanternApp)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.App
import Lantern.Query
import LanternShell.ArgumentParser as ArgumentParser
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { query : String
    , arguments : Dict String String
    , result : Maybe Bool
    }


type Message
    = Update String
    | UpdateArgument String String
    | HandleResult (Result Lantern.Error Lantern.Query.WriterResult)
    | Run


init : Model
init =
    { query = ""
    , arguments = Dict.empty
    , result = Nothing
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        Update query ->
            let
                argumentNames =
                    ArgumentParser.parse query

                arguments =
                    argumentNames
                        |> List.map (\n -> ( n, Dict.get n model.arguments |> Maybe.withDefault "" ))
                        |> Dict.fromList
            in
            ( { model | query = query, arguments = arguments }, Cmd.none )

        UpdateArgument name value ->
            ( { model | arguments = Dict.insert name value model.arguments }, Cmd.none )

        Run ->
            let
                query =
                    { source = model.query
                    , arguments = Dict.map (\_ v -> Lantern.Query.String v) model.arguments
                    }
            in
            ( model
            , Lantern.writerQuery
                query
                HandleResult
                |> Lantern.App.call
            )

        HandleResult result ->
            case result of
                Ok _ ->
                    ( { model | result = Just True }, Cmd.none )

                Err _ ->
                    ( { model | result = Just False }, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } { query, arguments } =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = Update >> Lantern.App.Message
            , text = query
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Writer query"
            }
        , Element.column []
            (arguments
                |> Dict.toList
                |> List.map
                    (\( name, value ) ->
                        LanternUi.Input.text theme
                            []
                            { onChange = UpdateArgument name >> Lantern.App.Message
                            , text = value
                            , placeholder = Nothing
                            , label = Element.Input.labelLeft [] (Element.text (name ++ ": "))
                            }
                    )
            )
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.App.Message Run)
            , label = Element.text "Run writer query"
            }
        ]


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Writer Query"
        , init = init
        , view = view
        , update = update
        }
