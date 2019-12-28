module DevTools.Apps.WriterQuery exposing (Message, Model, init, lanternApp, update, view)

import DevTools.ArgumentParser as ArgumentParser
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.Query
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


lanternApp : Lantern.App Context Model Message
lanternApp =
    Lantern.simpleApp
        { model = init
        , view = view
        , update = update
        }


type Message
    = Update String
    | UpdateArgument String String
    | HandleResult Bool
    | Run


init : Model
init =
    { query = ""
    , arguments = Dict.empty
    , result = Nothing
    }


update : Context -> Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
update _ msg model =
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
            )

        HandleResult result ->
            ( { model | result = Just result }, Cmd.none )


view : Context -> Model -> Element (Lantern.Message Message)
view { theme } { query, arguments } =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = Update >> Lantern.AppMessage
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
            , label = Element.text "Run writer query"
            }
        ]
