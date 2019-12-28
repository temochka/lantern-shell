port module DevTools exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Debug
import DevTools.Apps.ReaderQuery as ReaderQueryApp
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
import Keyboard.Event
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
import Task


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
    { writerQuery : String
    , writerQueryArguments : Dict String String
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
            Lantern.newConnection lanternRequestPort lanternResponsePort

        processTable =
            ProcessTable.empty
                |> ProcessTable.launch TableViewerApp
                |> ProcessTable.launch (ReaderQueryApp ReaderQueryApp.init)
                |> ProcessTable.launch WriterQueryApp

        model =
            { writerQuery = ""
            , writerQueryArguments = Dict.empty
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
                        [ ( "Run query", ReaderQueryApp ReaderQueryApp.init )
                        , ( "Run mutator", WriterQueryApp )
                        , ( "Run migration", MigrationApp )
                        , ( "Show tables", TableViewerApp )
                        , ( "Run echo", EchoApp )
                        , ( "Show logs", LogViewerApp )
                        ]
                    , placeholder = Nothing
                    }
                    |> LanternUi.FuzzySelect.setId "lanternAppLauncher"
            , theme = LanternUi.Theme.lightTheme
            }

        lanternCmd =
            liveQueries model |> Cmd.map LanternMessage
    in
    ( model
    , lanternCmd
    )



-- LIVE QUERIES


liveQueries : Model -> Cmd (Lantern.Message Msg)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ lanternResponsePort (Lantern.wrapResponse >> LanternMessage)
        , Browser.Events.onKeyDown handleShortcuts
        ]



-- APPS


type App
    = ReaderQueryApp ReaderQueryApp.Model
    | WriterQueryApp
    | MigrationApp
    | TableViewerApp
    | EchoApp
    | LogViewerApp


type AppMessage
    = ReaderQueryMsg ReaderQueryApp.Message


wrapAppMessage : ProcessTable.Pid -> Lantern.Message AppMessage -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.AppMessage appMsg ->
            AppMessage pid appMsg

        lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)


processAppMessage : ProcessTable.Pid -> AppMessage -> App -> ( App, Cmd Msg )
processAppMessage pid msg app =
    case ( app, msg ) of
        ( ReaderQueryApp model, ReaderQueryMsg appMsg ) ->
            ReaderQueryApp.update appMsg model |> Tuple.mapFirst ReaderQueryApp |> Tuple.mapSecond (Cmd.map (Lantern.map ReaderQueryMsg >> wrapAppMessage pid))

        _ ->
            ( app, Cmd.none )



-- UPDATE


type Msg
    = Nop
    | AppMessage ProcessTable.Pid AppMessage
    | NextWindow
    | PrevWindow
    | CloseApp
    | FocusAppLauncher
    | UpdateWriterQuery String
    | UpdateWriterQueryArgument String String
    | UpdatePing String
    | UpdateDdl String
    | RunWriterQuery
    | RunPing
    | RunDdl
    | DdlResult Bool
    | ReceivePong String
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
        Nop ->
            ( model, Cmd.none )

        NextWindow ->
            LanternUi.WindowManager.nextWindow model.windowManager Nop (\wm -> { model | windowManager = wm })

        PrevWindow ->
            LanternUi.WindowManager.prevWindow model.windowManager Nop (\wm -> { model | windowManager = wm })

        CloseApp ->
            let
                newProcessTable =
                    model.windowManager.focus
                        |> Maybe.map (\pid -> ProcessTable.kill pid model.processTable)
                        |> Maybe.withDefault model.processTable
            in
            LanternUi.WindowManager.syncProcesses
                (ProcessTable.pids newProcessTable)
                model.windowManager
                Nop
                (\wm -> { model | processTable = newProcessTable, windowManager = wm })

        FocusAppLauncher ->
            ( model, Browser.Dom.focus "lanternAppLauncher" |> Task.attempt (\_ -> Nop) )

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
            ( model, Lantern.echo model.ping ReceivePong |> Cmd.map LanternMessage )

        ReceivePong pong ->
            ( { model | pong = pong }, Cmd.none )

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
                |> Cmd.map LanternMessage
            )

        RunDdl ->
            let
                ddlQuery =
                    Lantern.Query.withNoArguments model.ddl
            in
            ( model, Lantern.migrate ddlQuery DdlResult |> Cmd.map LanternMessage )

        DdlResult r ->
            ( { model | ddlResult = r }, Cmd.none )

        WriterQueryResult _ ->
            ( model, Cmd.none )

        LanternMessage message ->
            case message of
                Lantern.Message lanternMessage ->
                    let
                        ( lanternConnection, lanternCmd ) =
                            Lantern.update lanternMessage model.lanternConnection
                    in
                    ( { model | lanternConnection = lanternConnection }, lanternCmd )

                Lantern.AppMessage appMessage ->
                    update appMessage model

        AppLauncherMessage proxiedMsg ->
            ( { model | appLauncher = LanternUi.FuzzySelect.update proxiedMsg model.appLauncher }, Cmd.none )

        LaunchApp app ->
            let
                newProcessTable =
                    ProcessTable.launch app model.processTable
            in
            LanternUi.WindowManager.syncProcesses
                (ProcessTable.pids newProcessTable)
                model.windowManager
                Nop
                (\wm ->
                    { model
                        | processTable = newProcessTable
                        , windowManager = wm
                        , appLauncher = LanternUi.FuzzySelect.reset model.appLauncher
                    }
                )

        LoadTable table ->
            let
                newTableViewerState =
                    TableViewer.loadTable model.tableViewer table

                newState =
                    { model | tableViewer = newTableViewerState }
            in
            ( newState
            , liveQueries newState |> Cmd.map LanternMessage
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

        AppMessage pid proxiedMsg ->
            let
                result =
                    pid
                        |> ProcessTable.lookup model.processTable
                        |> Maybe.map (ProcessTable.processApp >> processAppMessage pid proxiedMsg)

                newProcessTable =
                    case result of
                        Just ( app, _ ) ->
                            ProcessTable.mapProcess (always app) pid model.processTable

                        Nothing ->
                            model.processTable
            in
            ( { model | processTable = newProcessTable }, result |> Maybe.map Tuple.second |> Maybe.withDefault Cmd.none )


handleShortcuts : Json.Decode.Decoder Msg
handleShortcuts =
    let
        dispatchKeyPress keyPress =
            case ( keyPress.ctrlKey, keyPress.key ) of
                ( True, Just "." ) ->
                    FocusAppLauncher

                ( True, Just "j" ) ->
                    NextWindow

                ( True, Just "k" ) ->
                    PrevWindow

                ( True, Just "w" ) ->
                    CloseApp

                _ ->
                    Nop
    in
    Keyboard.Event.decodeKeyboardEvent
        |> Json.Decode.map dispatchKeyPress



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
                ReaderQueryApp appModel ->
                    ReaderQueryApp.view model.theme appModel
                        |> List.map (\e -> Element.map (Lantern.map ReaderQueryMsg >> wrapAppMessage process.pid) e)

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
            , Element.clipX
            ]
            [ renderAppLauncher model, tools model ]
        )
