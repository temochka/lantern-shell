module DevTools.Apps.ReaderQuery exposing (Message, Model, init, update, view)

import DevTools.ArgumentParser as ArgumentParser
import DevTools.FlexiQuery as FlexiQuery
import DevTools.Ui.ResultsTable as ResultsTable
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.Query
import LanternUi.Input
import LanternUi.Theme


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


update : Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
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
            )

        HandleResult result ->
            case result of
                Err error ->
                    ( { model | result = Just (Err error) }, Cmd.none )

                Ok queryResult ->
                    ( { model | result = Just (Ok queryResult) }, Cmd.none )


view : LanternUi.Theme.Theme -> Model -> List (Element (Lantern.Message Message))
view theme model =
    [ LanternUi.Input.multiline theme
        []
        { onChange = Update >> Lantern.AppMessage
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
                        { onChange = UpdateArgument name >> Lantern.AppMessage
                        , text = value
                        , placeholder = Nothing
                        , label = Element.Input.labelLeft [] (Element.text (name ++ ": "))
                        }
                )
        )
    , LanternUi.Input.button theme
        []
        { onPress = Just (Lantern.AppMessage Run)
        , label = Element.text "Run reader query"
        }
    , ResultsTable.render (model.result |> Maybe.withDefault (Ok []) |> Result.withDefault [])
    ]
