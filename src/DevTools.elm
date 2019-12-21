port module DevTools exposing (..)

import Browser
import Debug
import DevTools.ArgumentParser as ArgumentParser
import DevTools.FlexiQuery as FlexiQuery
import DevTools.TableViewer as TableViewer
import DevTools.Ui.StatusBar as StatusBar exposing (StatusBar)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html exposing (Html)
import Json.Decode
import Json.Encode
import Lantern
import Lantern.Encoders
import Lantern.Log
import Lantern.Query
import Lantern.Request
import LanternUi
import LanternUi.FuzzySelect
import LanternUi.Input
import LanternUi.Theme exposing (lightTheme)
import LanternUi.WindowManager
import ProcessTable exposing (ProcessTable)
import String


port lanternRequestPort : Lantern.RequestPort msg


port lanternResponsePort : Lantern.ResponsePort msg



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Table =
    { name : String }


tableDecoder : Json.Decode.Decoder Table
tableDecoder =
    Json.Decode.map
        Table
        (Json.Decode.field "name" Json.Decode.string)


type alias Model =
    { readerQuery : String
    , readerQueryArguments : Dict String String
    , writerQuery : String
    , writerQueryArguments : Dict String String
    , queryResult : List FlexiQuery.Result
    , queryError : Maybe Lantern.Error
    , databaseTables : List Table
    , tableViewerRows : List FlexiQuery.Result
    , tableViewerCount : Int
    , tableViewer : TableViewer.TableViewer
    , ddl : String
    , ddlResult : Bool
    , ddlError : Maybe Lantern.Error
    , ping : String
    , pong : String
    , serverResponse : Maybe String
    , lanternConnection : Lantern.Connection Msg
    , statusBar : StatusBar
    , processTable : ProcessTable App
    , windowManager : LanternUi.WindowManager.WindowManager
    , appLauncher : LanternUi.FuzzySelect.FuzzySelect App
    , theme : LanternUi.Theme.Theme
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        tableViewer =
            TableViewer.init

        lanternConnection =
            Lantern.newConnection lanternRequestPort lanternResponsePort LanternMessage

        processTable =
            ProcessTable.empty
                |> ProcessTable.launch TableViewerApp
                |> ProcessTable.launch ReaderQueryApp
                |> ProcessTable.launch WriterQueryApp

        model =
            { readerQuery = ""
            , readerQueryArguments = Dict.empty
            , writerQuery = ""
            , writerQueryArguments = Dict.empty
            , queryResult = []
            , queryError = Nothing
            , databaseTables = []
            , tableViewerRows = []
            , tableViewerCount = 0
            , tableViewer = tableViewer
            , ddl = ""
            , ddlResult = False
            , ddlError = Nothing
            , ping = ""
            , pong = ""
            , serverResponse = Nothing
            , lanternConnection = lanternConnection
            , statusBar = StatusBar.new
            , processTable = processTable
            , windowManager = LanternUi.WindowManager.new (ProcessTable.pids processTable)
            , appLauncher =
                LanternUi.FuzzySelect.new
                    { options =
                        [ ( "Run query", ReaderQueryApp )
                        , ( "Run mutator", WriterQueryApp )
                        , ( "Run migration", MigrationApp )
                        , ( "Show tables", TableViewerApp )
                        , ( "Run echo", EchoApp )
                        , ( "Show logs", LogViewerApp )
                        ]
                    , placeholder = Nothing
                    }
            , theme = LanternUi.Theme.lightTheme
            }

        lanternCmd =
            liveQueries model
    in
    ( model
    , lanternCmd
    )



-- LIVE QUERIES


liveQueries : Model -> Cmd Msg
liveQueries model =
    let
        tablesQuery =
            Lantern.prepareLiveQuery ( Lantern.Query.Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" Dict.empty, tableDecoder ) UpdateTables
                |> Just

        tableViewerQuery =
            TableViewer.liveQuery model.tableViewer UpdateTableRows
    in
    Lantern.liveQueries
        ([ tablesQuery, tableViewerQuery ] |> List.filterMap identity)
        model.lanternConnection



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Lantern.subscriptions model.lanternConnection



-- APPS


type App
    = ReaderQueryApp
    | WriterQueryApp
    | MigrationApp
    | TableViewerApp
    | EchoApp
    | LogViewerApp



-- UPDATE


type Msg
    = UpdateReaderQuery String
    | UpdateReaderQueryArgument String String
    | UpdateWriterQuery String
    | UpdateWriterQueryArgument String String
    | UpdatePing String
    | UpdateDdl String
    | RunReaderQuery
    | RunWriterQuery
    | RunPing
    | RunDdl
    | DdlResult Bool
    | ReceivePong String
    | ReaderQueryResult (Result Lantern.Error (List FlexiQuery.Result))
    | WriterQueryResult Bool
    | LanternMessage (Lantern.Message Msg)
    | AppLauncherMessage (LanternUi.FuzzySelect.Message App)
    | LoadTable String
    | UpdateTableRows (Result Lantern.Error ( List FlexiQuery.Result, List Int ))
    | UpdateTables (Result Lantern.Error (List Table))
    | UpdateStatusBar StatusBar.Message
    | LaunchApp App
    | WindowManagerMessage LanternUi.WindowManager.Message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateReaderQuery query ->
            let
                argumentNames =
                    ArgumentParser.parse query

                arguments =
                    argumentNames
                        |> List.map (\n -> ( n, Dict.get n model.readerQueryArguments |> Maybe.withDefault "" ))
                        |> Dict.fromList
            in
            ( { model | readerQuery = query, readerQueryArguments = arguments }, Cmd.none )

        UpdateReaderQueryArgument name value ->
            ( { model | readerQueryArguments = Dict.insert name value model.readerQueryArguments }, Cmd.none )

        UpdateWriterQuery query ->
            let
                argumentNames =
                    ArgumentParser.parse query

                arguments =
                    argumentNames
                        |> List.map (\n -> ( n, Dict.get n model.writerQueryArguments |> Maybe.withDefault "" ))
                        |> Dict.fromList
            in
            ( { model | writerQuery = query, writerQueryArguments = arguments }, Cmd.none )

        UpdateWriterQueryArgument name value ->
            ( { model | writerQueryArguments = Dict.insert name value model.writerQueryArguments }, Cmd.none )

        UpdatePing ping ->
            ( { model | ping = ping }, Cmd.none )

        UpdateDdl ddl ->
            ( { model | ddl = ddl }, Cmd.none )

        RunPing ->
            ( model, Lantern.echo model.ping ReceivePong model.lanternConnection )

        ReceivePong pong ->
            ( { model | pong = pong }, Cmd.none )

        RunReaderQuery ->
            let
                query =
                    { source = model.readerQuery
                    , arguments = Dict.map (\_ v -> Lantern.Query.String v) model.readerQueryArguments
                    }
            in
            ( model
            , Lantern.readerQuery
                query
                FlexiQuery.resultDecoder
                ReaderQueryResult
                model.lanternConnection
            )

        RunWriterQuery ->
            let
                query =
                    { source = model.writerQuery
                    , arguments = Dict.map (\_ v -> Lantern.Query.String v) model.writerQueryArguments
                    }
            in
            ( model
            , Lantern.writerQuery
                query
                WriterQueryResult
                model.lanternConnection
            )

        RunDdl ->
            let
                ddlQuery =
                    Lantern.Query.withNoArguments model.ddl
            in
            ( model, Lantern.migrate ddlQuery DdlResult model.lanternConnection )

        DdlResult r ->
            ( { model | ddlResult = r }, Cmd.none )

        ReaderQueryResult result ->
            case result of
                Err error ->
                    ( { model | queryError = Just error }, Cmd.none )

                Ok queryResult ->
                    ( { model | queryResult = queryResult }, Cmd.none )

        WriterQueryResult _ ->
            ( model, Cmd.none )

        LanternMessage message ->
            let
                ( lanternConnection, lanternCmd ) =
                    Lantern.update message model.lanternConnection
            in
            ( { model | lanternConnection = lanternConnection }, lanternCmd )

        AppLauncherMessage proxiedMsg ->
            ( { model | appLauncher = LanternUi.FuzzySelect.update proxiedMsg model.appLauncher }, Cmd.none )

        LaunchApp app ->
            let
                newProcessTable =
                    ProcessTable.launch app model.processTable
            in
            ( { model
                | processTable = newProcessTable
                , windowManager = LanternUi.WindowManager.syncProcesses (ProcessTable.pids newProcessTable) model.windowManager
                , appLauncher = LanternUi.FuzzySelect.reset model.appLauncher
              }
            , Cmd.none
            )

        LoadTable table ->
            let
                newTableViewerState =
                    TableViewer.loadTable model.tableViewer table

                newState =
                    { model | tableViewer = newTableViewerState }
            in
            ( newState
            , liveQueries newState
            )

        UpdateTables result ->
            case result of
                Err err ->
                    Debug.todo "implement error handling"

                Ok tables ->
                    ( { model | databaseTables = tables }, Cmd.none )

        UpdateTableRows result ->
            case result of
                Err err ->
                    Debug.todo "implement error handling"

                Ok ( rows, count ) ->
                    let
                        newTableViewer =
                            TableViewer.loadRows
                                model.tableViewer
                                rows
                                (count |> List.head |> Maybe.withDefault 0)
                    in
                    ( { model | tableViewer = newTableViewer }, Cmd.none )

        UpdateStatusBar statusBarMsg ->
            StatusBar.update statusBarMsg model.statusBar
                |> Tuple.mapFirst (\statusBar -> { model | statusBar = statusBar })

        WindowManagerMessage proxiedMsg ->
            let
                newWindowManager =
                    LanternUi.WindowManager.update proxiedMsg model.windowManager
            in
            ( { model | windowManager = newWindowManager }, Cmd.none )



-- VIEW


renderLogViewerApp : Model -> List (Element Msg)
renderLogViewerApp model =
    [ model.lanternConnection
        |> Lantern.log
        |> .lines
        |> List.map (\( _, line ) -> Element.text line)
        |> Element.column
            [ Element.width Element.fill
            , LanternUi.listSpacing
            , Element.Font.family [ Element.Font.typeface "Monaco", Element.Font.typeface "Fira Mono", Element.Font.monospace ]
            ]
    ]


resultsTable : List FlexiQuery.Result -> Element Msg
resultsTable results =
    let
        valueToString val =
            case val of
                Lantern.Query.Null ->
                    ""

                Lantern.Query.Integer i ->
                    String.fromInt i

                Lantern.Query.Real r ->
                    String.fromFloat r

                Lantern.Query.Text t ->
                    t

        columns =
            results
                |> List.head
                |> Maybe.map (Dict.keys >> List.sort)
                |> Maybe.withDefault []
                |> List.map
                    (\title ->
                        { header = Element.text title
                        , width = Element.fill
                        , view = \row -> Dict.get title row |> Maybe.map valueToString |> Maybe.withDefault "" |> Element.text
                        }
                    )
    in
    Element.table []
        { data = results
        , columns = columns
        }


renderReaderQueryApp : Model -> List (Element Msg)
renderReaderQueryApp model =
    [ LanternUi.Input.multiline model.theme
        []
        { onChange = UpdateReaderQuery
        , text = model.readerQuery
        , placeholder = Nothing
        , spellcheck = False
        , label = Element.Input.labelHidden "Reader query"
        }
    , Element.column []
        (model.readerQueryArguments
            |> Dict.toList
            |> List.map
                (\( name, value ) ->
                    LanternUi.Input.text model.theme
                        []
                        { onChange = UpdateReaderQueryArgument name
                        , text = value
                        , placeholder = Nothing
                        , label = Element.Input.labelLeft [] (Element.text (name ++ ": "))
                        }
                )
        )
    , LanternUi.Input.button model.theme
        []
        { onPress = Just RunReaderQuery
        , label = Element.text "Run reader query"
        }
    , resultsTable model.queryResult
    , Element.text ("Server error: " ++ (model.queryError |> Maybe.map Lantern.errorToString |> Maybe.withDefault ""))
    ]


renderWriterQueryApp : Model -> List (Element Msg)
renderWriterQueryApp model =
    [ LanternUi.Input.multiline model.theme
        []
        { onChange = UpdateWriterQuery
        , text = model.writerQuery
        , placeholder = Nothing
        , spellcheck = False
        , label = Element.Input.labelHidden "Writer query"
        }
    , Element.column []
        (model.writerQueryArguments
            |> Dict.toList
            |> List.map
                (\( name, value ) ->
                    LanternUi.Input.text model.theme
                        []
                        { onChange = UpdateWriterQueryArgument name
                        , text = value
                        , placeholder = Nothing
                        , label = Element.Input.labelLeft [] (Element.text (name ++ ": "))
                        }
                )
        )
    , LanternUi.Input.button model.theme
        []
        { onPress = Just RunWriterQuery
        , label = Element.text "Run writer query"
        }
    ]


renderMigrationApp : Model -> List (Element Msg)
renderMigrationApp model =
    [ LanternUi.Input.multiline model.theme
        []
        { onChange = UpdateDdl
        , text = model.ddl
        , placeholder = Nothing
        , spellcheck = False
        , label = Element.Input.labelHidden "Migration"
        }
    , LanternUi.Input.button model.theme
        []
        { onPress = Just RunDdl
        , label = Element.text "Run DDL"
        }
    ]


renderTableViewerApp : Model -> List (Element Msg)
renderTableViewerApp model =
    let
        tableList =
            model.databaseTables
                |> List.map (\{ name } -> Element.Input.button [] { label = Element.text name, onPress = Just (LoadTable name) })
    in
    [ Element.row [ Element.width Element.fill ]
        [ Element.column [ Element.width (Element.fillPortion 2) ] tableList
        , Element.el [ Element.width (Element.fillPortion 5) ] (TableViewer.render model.tableViewer)
        ]
    ]


renderEchoApp : Model -> List (Element Msg)
renderEchoApp model =
    [ LanternUi.Input.multiline model.theme
        []
        { onChange = UpdatePing
        , text = model.ping
        , placeholder = Nothing
        , spellcheck = False
        , label = Element.Input.labelHidden "Echo"
        }
    , LanternUi.Input.button model.theme
        []
        { onPress = Just RunPing
        , label = Element.text "Run echo"
        }
    , Element.text ("Results: " ++ Debug.toString model.pong)
    ]


renderApp : Model -> Bool -> ProcessTable.Process App -> Element Msg
renderApp model focused process =
    let
        content =
            case ProcessTable.processApp process of
                ReaderQueryApp ->
                    renderReaderQueryApp model

                WriterQueryApp ->
                    renderWriterQueryApp model

                MigrationApp ->
                    renderMigrationApp model

                TableViewerApp ->
                    renderTableViewerApp model

                EchoApp ->
                    renderEchoApp model

                LogViewerApp ->
                    renderLogViewerApp model

        border =
            if focused then
                Element.Border.shadow
                    { offset = ( 0.0, 0.0 )
                    , size = 0.1
                    , blur = 4.0
                    , color = model.theme.panelShadow
                    }

            else
                LanternUi.noneAttribute
    in
    LanternUi.panel model.theme
        [ border ]
        content


tools : Model -> Element Msg
tools model =
    let
        wrapRender pid focused =
            pid
                |> ProcessTable.lookup model.processTable
                |> Maybe.map (renderApp model focused)
                |> Maybe.withDefault Element.none
    in
    LanternUi.WindowManager.render { spacing = 5, padding = 0 } wrapRender WindowManagerMessage model.windowManager


renderAppLauncher : Model -> Element Msg
renderAppLauncher model =
    Element.row
        [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.text ">"
        , LanternUi.FuzzySelect.render lightTheme model.appLauncher AppLauncherMessage LaunchApp
        ]


view : Model -> Html Msg
view model =
    StatusBar.render model.statusBar
        UpdateStatusBar
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 15
            , Element.spacing 15
            ]
            [ renderAppLauncher model, tools model ]
        )
