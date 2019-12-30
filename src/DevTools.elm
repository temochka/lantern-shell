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
                |> ProcessTable.launch (DatabaseExplorerApp DatabaseExplorerApp.lanternApp)
                |> ProcessTable.launch (ReaderQueryApp ReaderQueryApp.lanternApp)
                |> ProcessTable.launch (WriterQueryApp WriterQueryApp.lanternApp)

        model =
            { lanternConnection = lanternConnection
            , processTable = processTable
            , windowManager = LanternUi.WindowManager.new (ProcessTable.pids processTable)
            , appLauncher =
                LanternUi.FuzzySelect.new
                    { options =
                        [ ( "Run query", ReaderQueryApp ReaderQueryApp.lanternApp )
                        , ( "Run mutator", WriterQueryApp WriterQueryApp.lanternApp )
                        , ( "Run migration", MigrationApp MigrationsApp.lanternApp )
                        , ( "Show tables", DatabaseExplorerApp DatabaseExplorerApp.lanternApp )
                        , ( "Run echo", EchoApp EchoApp.lanternApp )
                        , ( "Show logs", LogViewerApp LogViewerApp.lanternApp )
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
    = ReaderQueryApp ReaderQueryApp.App
    | WriterQueryApp WriterQueryApp.App
    | MigrationApp MigrationsApp.App
    | DatabaseExplorerApp DatabaseExplorerApp.App
    | EchoApp EchoApp.App
    | LogViewerApp LogViewerApp.App


type AppMessage
    = ReaderQueryMsg ReaderQueryApp.Message
    | WriterQueryMsg WriterQueryApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | EchoMsg EchoApp.Message
    | LogViewerMsg LogViewerApp.Message



-- type AppContext
--     = ReaderQueryContext ReaderQueryApp.Context
--     | WriterQueryContext WriterQueryApp.Context
--     | DatabaseExporerContext DatabaseExplorerApp.Context
--     | MigrationsContext MigrationsApp.Context
--     | Echo


wrapAppMessage : ProcessTable.Pid -> Lantern.Message AppMessage -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.AppMessage appMsg ->
            AppMessage pid appMsg

        lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)


processAppMessage : Model -> ProcessTable.Pid -> AppMessage -> Result String ( App, Cmd Msg )
processAppMessage model pid msg =
    let
        updateApp appMsg appState context wrapApp wrapMsg =
            appState.update context appMsg appState.model
                |> Tuple.mapFirst (\m -> wrapApp { appState | model = m })
                |> Tuple.mapSecond (Cmd.map (Lantern.map wrapMsg >> wrapAppMessage pid))
                |> Ok
    in
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
                    ( ReaderQueryApp appState, ReaderQueryMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme } ReaderQueryApp ReaderQueryMsg

                    ( WriterQueryApp appState, WriterQueryMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme } WriterQueryApp WriterQueryMsg

                    ( MigrationApp appState, MigrationsMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme } MigrationApp MigrationsMsg

                    ( DatabaseExplorerApp appState, DatabaseExplorerMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme } DatabaseExplorerApp DatabaseExplorerMsg

                    ( EchoApp appState, EchoMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme } EchoApp EchoMsg

                    ( LogViewerApp appState, LogViewerMsg appMsg ) ->
                        updateApp appMsg appState { theme = model.theme, log = Lantern.log model.lanternConnection } LogViewerApp LogViewerMsg

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
        render app context wrapMsg =
            app.view context app.model
                |> Element.map (Lantern.map wrapMsg >> wrapAppMessage process.pid)

        themeContext =
            { theme = model.theme }

        content =
            case ProcessTable.processApp process of
                ReaderQueryApp app ->
                    render app themeContext ReaderQueryMsg

                WriterQueryApp app ->
                    render app themeContext WriterQueryMsg

                MigrationApp app ->
                    render app themeContext MigrationsMsg

                DatabaseExplorerApp app ->
                    render app themeContext DatabaseExplorerMsg

                EchoApp app ->
                    render app themeContext EchoMsg

                LogViewerApp app ->
                    render app { theme = model.theme, log = Lantern.log model.lanternConnection } LogViewerMsg

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
    LanternUi.columnLayout
        model.theme
        []
        [ renderAppLauncher model, tools model ]
        |> Element.layout [ Element.width Element.fill, Element.padding 20 ]
