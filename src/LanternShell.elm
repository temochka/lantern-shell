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
import LanternShell.Apps
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


appContext : Model -> LanternShell.Apps.Context Msg
appContext { theme, lanternConnection } =
    { theme = theme, lanternConnection = lanternConnection }



-- MODEL


type alias LauncherEntry =
    LanternShell.Apps.Context Msg -> ( LanternShell.Apps.App, Cmd (Lantern.App.Message LanternShell.Apps.Message) )


type alias Model =
    { lanternConnection : Lantern.Connection Msg
    , processTable : ProcessTable LanternShell.Apps.App
    , windowManager : LanternUi.WindowManager.WindowManager
    , appLauncher : LanternUi.FuzzySelect.FuzzySelect LauncherEntry
    , theme : LanternUi.Theme.Theme
    , liveQueriesCache : Dict ProcessTable.Pid (List (LiveQuery Msg))
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        lanternConnection =
            Lantern.newConnection lanternRequestPort

        initialTheme =
            LanternUi.Theme.lightTheme

        bootContext =
            { theme = initialTheme, lanternConnection = lanternConnection }

        preloadedApps =
            [ LanternShell.Apps.databaseExplorer
            , LanternShell.Apps.readerQuery
            , LanternShell.Apps.writerQuery
            ]
                |> List.map (\app -> (app bootContext).init)

        processTable =
            List.foldl (\( app, _ ) newProcessTable -> ProcessTable.launch app newProcessTable) ProcessTable.empty preloadedApps

        preloadedAppsCmds =
            Cmd.batch (List.map Tuple.second preloadedApps)

        model =
            { lanternConnection = lanternConnection
            , processTable = processTable
            , windowManager = LanternUi.WindowManager.new (ProcessTable.pids processTable)
            , appLauncher =
                LanternUi.FuzzySelect.new
                    { options =
                        [ ( "Run query", \context -> (LanternShell.Apps.readerQuery context).init )
                        , ( "Run mutator", \context -> (LanternShell.Apps.writerQuery context).init )
                        , ( "Run migration", \context -> (LanternShell.Apps.migrations context).init )
                        , ( "Show tables", \context -> (LanternShell.Apps.databaseExplorer context).init )
                        , ( "Run echo", \context -> (LanternShell.Apps.echo context).init )
                        , ( "Show logs", \context -> (LanternShell.Apps.logViewer context).init )
                        ]
                    , placeholder = Nothing
                    }
                    |> LanternUi.FuzzySelect.setId "lanternAppLauncher"
            , theme = initialTheme
            , liveQueriesCache = Dict.empty
            }

        liveQueriesCache =
            processTable
                |> ProcessTable.processes
                |> List.map
                    (\process ->
                        ( process.pid
                        , (LanternShell.Apps.lanternAppFor process.application bootContext).liveQueries process.application
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
        [ Lantern.subscriptions LanternMessage lanternResponsePort
        , Browser.Events.onKeyDown (handleShortcuts model)
        ]



-- UPDATE


type Msg
    = Nop
    | AppLauncherMessage (LanternUi.FuzzySelect.Message LauncherEntry)
    | AppMessage ProcessTable.Pid LanternShell.Apps.Message
    | CloseApp ProcessTable.Pid
    | FocusAppLauncher
    | LanternMessage (Lantern.Message Msg)
    | LaunchApp LauncherEntry
    | WindowManagerMessage LanternUi.WindowManager.Message


threadModel : model -> List (model -> ( model, Cmd msg )) -> ( model, Cmd msg )
threadModel model ops =
    List.foldl (\updateFn ( oldModel, oldCmd ) -> updateFn oldModel |> Tuple.mapSecond (\newCmd -> Cmd.batch [ oldCmd, newCmd ])) ( model, Cmd.none ) ops


wrapAppMessage : ProcessTable.Pid -> Lantern.App.Message LanternShell.Apps.Message -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.App.Message appMsg ->
            AppMessage pid appMsg

        Lantern.App.LanternMessage lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        CloseApp pid ->
            let
                newProcessTable =
                    ProcessTable.kill pid model.processTable
            in
            [ update (WindowManagerMessage (LanternUi.WindowManager.SyncProcesses (ProcessTable.pids newProcessTable))) ]
                |> threadModel { model | processTable = newProcessTable }

        FocusAppLauncher ->
            ( model, Browser.Dom.focus "lanternAppLauncher" |> Task.attempt (\_ -> Nop) )

        LanternMessage lanternMessage ->
            let
                ( lanternConnection, lanternCmd ) =
                    Lantern.update lanternMessage model.lanternConnection
            in
            ( { model | lanternConnection = lanternConnection }, lanternCmd )

        AppLauncherMessage proxiedMsg ->
            ( { model | appLauncher = LanternUi.FuzzySelect.update proxiedMsg model.appLauncher }, Cmd.none )

        LaunchApp launcher ->
            let
                ( app, appCmd ) =
                    launcher (appContext model)

                newProcessTable =
                    ProcessTable.launch app model.processTable
            in
            [ \_ -> ( { model | processTable = newProcessTable }, Cmd.map (wrapAppMessage newProcessTable.pid) appCmd )
            , update (WindowManagerMessage (LanternUi.WindowManager.SyncProcesses (ProcessTable.pids newProcessTable)))
            ]
                |> threadModel model

        WindowManagerMessage proxiedMsg ->
            let
                ( newWindowManager, cmd ) =
                    LanternUi.WindowManager.update proxiedMsg model.windowManager
            in
            ( { model | windowManager = newWindowManager }, Cmd.map WindowManagerMessage cmd )

        AppMessage pid proxiedMsg ->
            pid
                |> ProcessTable.lookup model.processTable
                |> Maybe.map ProcessTable.processApp
                |> Maybe.map
                    (\appModel ->
                        let
                            lanternApp =
                                LanternShell.Apps.lanternAppFor appModel (appContext model)

                            ( newAppModel, appCmd ) =
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
                        , Cmd.batch [ Cmd.map (wrapAppMessage pid) appCmd, liveQueriesCmd ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )


handleShortcuts : Model -> Json.Decode.Decoder Msg
handleShortcuts model =
    let
        dispatchKeyPress keyPress =
            case ( keyPress.ctrlKey, keyPress.key ) of
                ( True, Just "." ) ->
                    FocusAppLauncher

                ( True, Just "j" ) ->
                    WindowManagerMessage LanternUi.WindowManager.NextWindow

                ( True, Just "k" ) ->
                    WindowManagerMessage LanternUi.WindowManager.PrevWindow

                ( True, Just "w" ) ->
                    model.windowManager.focus |> Maybe.map CloseApp |> Maybe.withDefault Nop

                _ ->
                    Nop
    in
    Keyboard.Event.decodeKeyboardEvent
        |> Json.Decode.map dispatchKeyPress



-- VIEW


renderApp : Model -> Bool -> ProcessTable.Process LanternShell.Apps.App -> Element Msg
renderApp model focused process =
    let
        app =
            ProcessTable.processApp process

        content =
            (LanternShell.Apps.lanternAppFor app (appContext model)).view () app

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
        |> Element.map (wrapAppMessage process.pid)


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
