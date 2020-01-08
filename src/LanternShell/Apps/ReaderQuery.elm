module LanternShell.Apps.ReaderQuery exposing (Message, Model, init, lanternApp)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.App
import Lantern.Query
import LanternShell.ArgumentParser as ArgumentParser
import LanternShell.FlexiQuery as FlexiQuery
import LanternShell.Ui.ResultsTable as ResultsTable
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { query : String
    , arguments : Dict String String
    , result : Maybe (Result Lantern.Error (List FlexiQuery.Result))
    }


init : Model
init =
    { query = ""
    , arguments = Dict.empty
    , result = Nothing
    }


type Message
    = Update String
    | UpdateArgument String String
    | HandleResult (Result Lantern.Error (List FlexiQuery.Result))
    | Run


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
            , Lantern.readerQuery
                query
                FlexiQuery.resultDecoder
                HandleResult
                |> Lantern.App.call
            )

        HandleResult result ->
            case result of
                Err error ->
                    ( { model | result = Just (Err error) }, Cmd.none )

                Ok queryResult ->
                    ( { model | result = Just (Ok queryResult) }, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } model =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = Update >> Lantern.App.Message
            , text = model.query
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Reader query"
            }
        , Element.column []
            (model.arguments
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
            , label = Element.text "Run reader query"
            }
        , ResultsTable.render (model.result |> Maybe.withDefault (Ok []) |> Result.withDefault [])
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { init = init
        , view = view
        , update = update
        }
