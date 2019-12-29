port module DevTools exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Debug
import DevTools.Apps.DatabaseExplorer as DatabaseExplorerApp
import DevTools.Apps.Echo as EchoApp
import DevTools.Apps.LogViewer as LogViewerApp
import DevTools.Apps.Migrations as MigrationsApp
import DevTools.Apps.ReaderQuery as ReaderQueryApp
import DevTools.Apps.WriterQuery as WriterQueryApp
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


type alias Model =
    { lanternConnection : Lantern.Connection Msg
    , statusBar : StatusBar
    , processTable : ProcessTable App
    , windowManager : LanternUi.WindowManager.WindowManager
    , appLauncher : LanternUi.FuzzySelect.FuzzySelect App
    , theme : LanternUi.Theme.Theme
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        lanternConnection =
            Lantern.newConnection lanternRequestPort lanternResponsePort

        processTable =
            ProcessTable.empty
                |> ProcessTable.launch (DatabaseExplorerApp DatabaseExplorerApp.init)
                |> ProcessTable.launch (ReaderQueryApp ReaderQueryApp.init)
                |> ProcessTable.launch (WriterQueryApp WriterQueryApp.init)

        model =
            { lanternConnection = lanternConnection
            , statusBar = StatusBar.new
            , processTable = processTable
            , windowManager = LanternUi.WindowManager.new (ProcessTable.pids processTable)
            , appLauncher =
                LanternUi.FuzzySelect.new
                    { options =
                        [ ( "Run query", ReaderQueryApp ReaderQueryApp.init )
                        , ( "Run mutator", WriterQueryApp WriterQueryApp.init )
                        , ( "Run migration", MigrationApp MigrationsApp.init )
                        , ( "Show tables", DatabaseExplorerApp DatabaseExplorerApp.init )
                        , ( "Run echo", EchoApp EchoApp.init )
                        , ( "Show logs", LogViewerApp LogViewerApp.init )
                        ]
                    , placeholder = Nothing
                    }
                    |> LanternUi.FuzzySelect.setId "lanternAppLauncher"
            , theme = LanternUi.Theme.lightTheme
            }
    in
    ( model
    , Cmd.none
    )



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
    | WriterQueryApp WriterQueryApp.Model
    | MigrationApp MigrationsApp.Model
    | DatabaseExplorerApp DatabaseExplorerApp.Model
    | EchoApp EchoApp.Model
    | LogViewerApp LogViewerApp.Model


type AppMessage
    = ReaderQueryMsg ReaderQueryApp.Message
    | WriterQueryMsg WriterQueryApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | EchoMsg EchoApp.Message
    | LogViewerMsg LogViewerApp.Message


wrapAppMessage : ProcessTable.Pid -> Lantern.Message AppMessage -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.AppMessage appMsg ->
            AppMessage pid appMsg

        lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)


processAppMessage : Model -> ProcessTable.Pid -> AppMessage -> Result String ( App, Cmd Msg )
processAppMessage model pid msg =
    pid
        |> ProcessTable.lookup model.processTable
        |> Result.fromMaybe "Unknown process"
        |> Result.andThen
            (\process ->
                let
                    app =
                        ProcessTable.processApp process
                in
                case ( app, msg ) of
                    ( ReaderQueryApp appModel, ReaderQueryMsg appMsg ) ->
                        ReaderQueryApp.update { theme = model.theme } appMsg appModel
                            |> Tuple.mapFirst ReaderQueryApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map ReaderQueryMsg >> wrapAppMessage pid))
                            |> Ok

                    ( WriterQueryApp appModel, WriterQueryMsg appMsg ) ->
                        WriterQueryApp.update { theme = model.theme } appMsg appModel
                            |> Tuple.mapFirst WriterQueryApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map WriterQueryMsg >> wrapAppMessage pid))
                            |> Ok

                    ( MigrationApp appModel, MigrationsMsg appMsg ) ->
                        MigrationsApp.update { theme = model.theme } appMsg appModel
                            |> Tuple.mapFirst MigrationApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map MigrationsMsg >> wrapAppMessage pid))
                            |> Ok

                    ( DatabaseExplorerApp appModel, DatabaseExplorerMsg appMsg ) ->
                        DatabaseExplorerApp.update { theme = model.theme } appMsg appModel
                            |> Tuple.mapFirst DatabaseExplorerApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map DatabaseExplorerMsg >> wrapAppMessage pid))
                            |> Ok

                    ( EchoApp appModel, EchoMsg appMsg ) ->
                        EchoApp.update { theme = model.theme } appMsg appModel
                            |> Tuple.mapFirst EchoApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map EchoMsg >> wrapAppMessage pid))
                            |> Ok

                    ( LogViewerApp appModel, LogViewerMsg appMsg ) ->
                        LogViewerApp.update { theme = model.theme, log = Lantern.log model.lanternConnection } appMsg appModel
                            |> Tuple.mapFirst LogViewerApp
                            |> Tuple.mapSecond (Cmd.map (Lantern.map LogViewerMsg >> wrapAppMessage pid))
                            |> Ok

                    _ ->
                        Err "Process cannot handle message"
            )



-- UPDATE


type Msg
    = Nop
    | AppMessage ProcessTable.Pid AppMessage
    | NextWindow
    | PrevWindow
    | CloseApp
    | FocusAppLauncher
    | LanternMessage (Lantern.Message Msg)
    | AppLauncherMessage (LanternUi.FuzzySelect.Message App)
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
                    processAppMessage model pid proxiedMsg

                newProcessTable =
                    case result of
                        Ok ( app, _ ) ->
                            ProcessTable.mapProcess (always app) pid model.processTable

                        Err _ ->
                            model.processTable
            in
            ( { model | processTable = newProcessTable }, result |> Result.map Tuple.second |> Result.withDefault Cmd.none )


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


renderApp : Model -> Bool -> ProcessTable.Process App -> Element Msg
renderApp model focused process =
    let
        content =
            case ProcessTable.processApp process of
                ReaderQueryApp appModel ->
                    ReaderQueryApp.view { theme = model.theme } appModel
                        |> Element.map (Lantern.map ReaderQueryMsg >> wrapAppMessage process.pid)

                WriterQueryApp appModel ->
                    WriterQueryApp.view { theme = model.theme } appModel
                        |> Element.map (Lantern.map WriterQueryMsg >> wrapAppMessage process.pid)

                MigrationApp appModel ->
                    MigrationsApp.view { theme = model.theme } appModel
                        |> Element.map (Lantern.map MigrationsMsg >> wrapAppMessage process.pid)

                DatabaseExplorerApp appModel ->
                    DatabaseExplorerApp.view { theme = model.theme } appModel
                        |> Element.map (Lantern.map DatabaseExplorerMsg >> wrapAppMessage process.pid)

                EchoApp appModel ->
                    EchoApp.view { theme = model.theme } appModel
                        |> Element.map (Lantern.map EchoMsg >> wrapAppMessage process.pid)

                LogViewerApp appModel ->
                    LogViewerApp.view { theme = model.theme, log = Lantern.log model.lanternConnection } appModel
                        |> Element.map (Lantern.map LogViewerMsg >> wrapAppMessage process.pid)

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
