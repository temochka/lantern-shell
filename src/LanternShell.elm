port module LanternShell exposing (..)

import Browser
import Browser.Dom
import Browser.Events
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
import Lantern.App
import Lantern.Encoders
import Lantern.LiveQuery exposing (LiveQuery(..))
import Lantern.Log
import Lantern.Query
import Lantern.Request
import LanternShell.Apps.DatabaseExplorer as DatabaseExplorerApp
import LanternShell.Apps.Echo as EchoApp
import LanternShell.Apps.LogViewer as LogViewerApp
import LanternShell.Apps.Migrations as MigrationsApp
import LanternShell.Apps.ReaderQuery as ReaderQueryApp
import LanternShell.Apps.WriterQuery as WriterQueryApp
import LanternShell.ArgumentParser as ArgumentParser
import LanternShell.FlexiQuery as FlexiQuery
import LanternShell.TableViewer as TableViewer
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
    , liveQueriesCache : Dict ProcessTable.Pid (List (LiveQuery Msg))
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
            , processTable = processTable
            , windowManager = LanternUi.WindowManager.new (ProcessTable.pids processTable)
            , appLauncher =
                LanternUi.FuzzySelect.new
                    { options =
                        [ ( "Run query", ReaderQueryApp ReaderQueryApp.init )
                        , ( "Run mutator", WriterQueryApp WriterQueryApp.init )
                        , ( "Run migration", MigrationsApp MigrationsApp.init )
                        , ( "Show tables", DatabaseExplorerApp DatabaseExplorerApp.init )
                        , ( "Run echo", EchoApp EchoApp.init )
                        , ( "Show logs", LogViewerApp LogViewerApp.init )
                        ]
                    , placeholder = Nothing
                    }
                    |> LanternUi.FuzzySelect.setId "lanternAppLauncher"
            , theme = LanternUi.Theme.lightTheme
            , liveQueriesCache = Dict.empty
            }

        liveQueriesCache =
            processTable
                |> ProcessTable.processes
                |> List.map
                    (\process ->
                        ( process.pid
                        , (lanternAppFor process.application model).liveQueries process.application
                            |> List.map (Lantern.LiveQuery.map (AppMessage process.pid))
                        )
                    )
                |> Dict.fromList
    in
    ( { model | liveQueriesCache = liveQueriesCache }
    , Lantern.liveQueries (liveQueriesCache |> Dict.values |> List.concatMap identity) |> Cmd.map LanternMessage
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
    = EchoApp EchoApp.Model
    | DatabaseExplorerApp DatabaseExplorerApp.Model
    | LogViewerApp LogViewerApp.Model
    | MigrationsApp MigrationsApp.Model
    | ReaderQueryApp ReaderQueryApp.Model
    | WriterQueryApp WriterQueryApp.Model


type AppMessage
    = EchoMsg EchoApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | LogViewerMsg LogViewerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | ReaderQueryMsg ReaderQueryApp.Message
    | WriterQueryMsg WriterQueryApp.Message


lanternAppFor : App -> (Model -> Lantern.App.App () App AppMessage)
lanternAppFor app =
    case app of
        EchoApp _ ->
            echoApp

        DatabaseExplorerApp _ ->
            databaseExplorerApp

        LogViewerApp _ ->
            logViewerApp

        MigrationsApp _ ->
            migrationsApp

        ReaderQueryApp _ ->
            readerQueryApp

        WriterQueryApp _ ->
            writerQueryApp


databaseExplorerApp : Model -> Lantern.App.App () App AppMessage
databaseExplorerApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    DatabaseExplorerMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    DatabaseExplorerApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = DatabaseExplorerApp
        , wrapMsg = DatabaseExplorerMsg
        , context = \_ -> { theme = rootModel.theme }
        }
        DatabaseExplorerApp.lanternApp


echoApp : Model -> Lantern.App.App () App AppMessage
echoApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    EchoMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    EchoApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = EchoApp
        , wrapMsg = EchoMsg
        , context = \_ -> { theme = rootModel.theme }
        }
        EchoApp.lanternApp


logViewerApp : Model -> Lantern.App.App () App AppMessage
logViewerApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    LogViewerMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    LogViewerApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = LogViewerApp
        , wrapMsg = LogViewerMsg
        , context = \_ -> { theme = rootModel.theme, log = Lantern.log rootModel.lanternConnection }
        }
        LogViewerApp.lanternApp


migrationsApp : Model -> Lantern.App.App () App AppMessage
migrationsApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    MigrationsMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    MigrationsApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = MigrationsApp
        , wrapMsg = MigrationsMsg
        , context = \_ -> { theme = rootModel.theme }
        }
        MigrationsApp.lanternApp


readerQueryApp : Model -> Lantern.App.App () App AppMessage
readerQueryApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    ReaderQueryMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    ReaderQueryApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = ReaderQueryApp
        , wrapMsg = ReaderQueryMsg
        , context = \_ -> { theme = rootModel.theme }
        }
        ReaderQueryApp.lanternApp


writerQueryApp : Model -> Lantern.App.App () App AppMessage
writerQueryApp rootModel =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    WriterQueryMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    WriterQueryApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = WriterQueryApp
        , wrapMsg = WriterQueryMsg
        , context = \_ -> { theme = rootModel.theme }
        }
        WriterQueryApp.lanternApp


wrapAppMessage : ProcessTable.Pid -> Lantern.Message AppMessage -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.AppMessage appMsg ->
            AppMessage pid appMsg

        lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)



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
            pid
                |> ProcessTable.lookup model.processTable
                |> Maybe.map ProcessTable.processApp
                |> Maybe.map
                    (\appModel ->
                        let
                            lanternApp =
                                lanternAppFor appModel model

                            ( newAppModel, cmd ) =
                                lanternApp.update proxiedMsg appModel

                            newLiveQueries =
                                lanternApp.liveQueries newAppModel |> List.map (Lantern.LiveQuery.map (AppMessage pid))

                            newLiveQueriesCache =
                                Dict.insert pid newLiveQueries model.liveQueriesCache

                            liveQueriesChanged =
                                Dict.get pid model.liveQueriesCache
                                    |> Maybe.map (Lantern.LiveQuery.areEqualLists newLiveQueries >> not)
                                    |> Maybe.withDefault True

                            liveQueriesCmd =
                                if liveQueriesChanged then
                                    Lantern.liveQueries (newLiveQueriesCache |> Dict.values |> List.concatMap identity)
                                        |> Cmd.map LanternMessage

                                else
                                    Cmd.none
                        in
                        ( { model
                            | processTable = ProcessTable.mapProcess (always newAppModel) pid model.processTable
                            , liveQueriesCache = newLiveQueriesCache
                          }
                        , Cmd.batch [ Cmd.map (wrapAppMessage pid) cmd, liveQueriesCmd ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )


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
        app =
            ProcessTable.processApp process

        content =
            (lanternAppFor app model).view () app

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
        |> Element.map (Lantern.map (AppMessage process.pid) >> LanternMessage)


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
