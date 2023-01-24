port module LanternShell exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Html.Attributes
import Json.Decode
import Keyboard.Event
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery(..))
import LanternShell.Apps
import LanternUi
import LanternUi.Input
import LanternUi.Theme
import LanternUi.WindowManager
import ProcessTable exposing (ProcessTable)
import Task
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


port lanternRequestPort : Lantern.RequestPort msg


port lanternResponsePort : Lantern.ResponsePort msg



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


appContext : Model -> LanternShell.Apps.Context Msg
appContext { theme, lanternConnection } =
    { theme = theme, lanternConnection = lanternConnection }



-- MODEL


type alias Model =
    { lanternConnection : Lantern.Connection Msg
    , processTable : ProcessTable (LanternShell.Apps.App Msg)
    , windowManager : LanternUi.WindowManager.WindowManager
    , theme : LanternUi.Theme.Theme
    , liveQueriesCache : Dict ProcessTable.Pid (List (LiveQuery Msg))
    , liveQueriesCacheExpired : Bool
    , navigationKey : Browser.Navigation.Key
    , viewport : { width : Int, height : Int }
    }


init : { width : Int, height : Int } -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init viewport url key =
    let
        lanternConnection =
            Lantern.newConnection lanternRequestPort

        initialTheme =
            LanternUi.Theme.solarizedDark

        ( windowManager, launchers ) =
            url
                |> (Url.Parser.Query.string "s" |> Url.Parser.query |> Url.Parser.parse)
                |> Maybe.andThen identity
                |> Maybe.andThen
                    (LanternUi.WindowManager.deserialize
                        (\( id, flags ) ->
                            LanternShell.Apps.launcherForId id
                                |> Maybe.map (\launcher -> ( launcher, flags ))
                        )
                    )
                |> Maybe.withDefault ( LanternUi.WindowManager.new [], [] )

        starterModel =
            { lanternConnection = lanternConnection
            , windowManager = windowManager
            , processTable = ProcessTable.empty
            , theme = initialTheme
            , liveQueriesCache = Dict.empty
            , liveQueriesCacheExpired = False
            , navigationKey = key
            , viewport = viewport
            }
    in
    threadModel starterModel
        (List.map
            (\( launcher, flagsAsValue ) ->
                let
                    initializedLauncher =
                        launcher (LanternShell.Apps.appContext starterModel)
                in
                launchApp initializedLauncher (flagsAsValue |> Maybe.andThen initializedLauncher.decodeFlags)
            )
            (( LanternShell.Apps.appLauncher
             , Nothing
             )
                :: launchers
            )
            ++ [ fireLiveQueries ]
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        appSubs =
            model.processTable
                |> ProcessTable.processes
                |> List.map
                    (\appProcess ->
                        let
                            appModel =
                                ProcessTable.processApp appProcess
                        in
                        (LanternShell.Apps.lanternAppFor appModel (LanternShell.Apps.appContext model)).subscriptions appModel
                            |> Sub.map (wrapAppMessage appProcess.pid)
                    )
                |> Sub.batch
    in
    Sub.batch
        [ Lantern.subscriptions LanternMessage lanternResponsePort
        , Browser.Events.onKeyDown (handleShortcuts model)
        , Browser.Events.onResize UpdateViewport
        , appSubs
        ]



-- UPDATE


type Msg
    = Nop
    | AppMessage ProcessTable.Pid (LanternShell.Apps.Message Msg)
    | CloseApp ProcessTable.Pid
    | FocusAppLauncher
    | LanternMessage (Lantern.Message Msg)
    | LaunchApp (LanternShell.Apps.LauncherEntry Msg) (Maybe (LanternShell.Apps.Flags Msg))
    | WindowManagerMessage LanternUi.WindowManager.Message
    | UpdateViewport Int Int
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest


threadModel : model -> List (model -> ( model, Cmd msg )) -> ( model, Cmd msg )
threadModel model ops =
    List.foldl (\updateFn ( oldModel, oldCmd ) -> updateFn oldModel |> Tuple.mapSecond (\newCmd -> Cmd.batch [ oldCmd, newCmd ])) ( model, Cmd.none ) ops


wrapAppMessage : ProcessTable.Pid -> Lantern.App.Message (LanternShell.Apps.Message Msg) -> Msg
wrapAppMessage pid msg =
    case msg of
        Lantern.App.Message appMsg ->
            AppMessage pid appMsg

        Lantern.App.LanternMessage lanternMessage ->
            LanternMessage (Lantern.map (AppMessage pid) lanternMessage)

        Lantern.App.Close ->
            CloseApp pid

        Lantern.App.Reflag ->
            WindowManagerMessage LanternUi.WindowManager.Nop


refreshLiveQueries : ProcessTable.Pid -> Model -> ( Model, Cmd Msg )
refreshLiveQueries pid model =
    pid
        |> ProcessTable.lookup model.processTable
        |> Maybe.map ProcessTable.processApp
        |> Maybe.map
            (\appModel ->
                let
                    lanternApp =
                        LanternShell.Apps.lanternAppFor appModel (LanternShell.Apps.appContext model)

                    newLiveQueries =
                        lanternApp.liveQueries appModel |> List.map (Lantern.LiveQuery.map (AppMessage pid))

                    liveQueriesCache =
                        Dict.insert pid newLiveQueries model.liveQueriesCache

                    liveQueriesChanged =
                        Dict.get pid model.liveQueriesCache
                            |> Maybe.map (Lantern.LiveQuery.areEqualLists newLiveQueries >> not)
                            |> Maybe.withDefault True
                in
                ( { model
                    | liveQueriesCache = liveQueriesCache
                    , liveQueriesCacheExpired = liveQueriesChanged
                  }
                , Cmd.none
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


fireLiveQueries : Model -> ( Model, Cmd Msg )
fireLiveQueries model =
    let
        cmd =
            if model.liveQueriesCacheExpired then
                Lantern.liveQueries (model.liveQueriesCache |> Dict.values |> List.concat)
                    |> Task.perform identity
                    |> Cmd.map LanternMessage

            else
                Cmd.none
    in
    ( model, cmd )


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
            [ update (WindowManagerMessage (LanternUi.WindowManager.SyncProcesses (ProcessTable.userPids newProcessTable))) ]
                |> threadModel { model | processTable = newProcessTable }

        FocusAppLauncher ->
            ( model, Browser.Dom.focus "lanternAppLauncher" |> Task.attempt (\_ -> Nop) )

        LanternMessage lanternMessage ->
            let
                ( lanternConnection, lanternCmd ) =
                    Lantern.update lanternMessage model.lanternConnection
            in
            ( { model | lanternConnection = lanternConnection }, lanternCmd )

        LaunchApp launcher flags ->
            threadModel model
                [ launchApp launcher flags
                , fireLiveQueries
                ]

        WindowManagerMessage proxiedMsg ->
            let
                ( newWindowManager, cmd ) =
                    LanternUi.WindowManager.update proxiedMsg model.windowManager

                serialized =
                    LanternUi.WindowManager.serialize
                        (\pid ->
                            let
                                process =
                                    ProcessTable.lookup model.processTable pid

                                processName =
                                    process
                                        |> Maybe.map (ProcessTable.processApp >> LanternShell.Apps.appId)
                                        |> Maybe.withDefault ""

                                processFlags =
                                    process
                                        |> Maybe.andThen
                                            (\p ->
                                                let
                                                    launcher =
                                                        (p.application |> LanternShell.Apps.lanternAppFor) (appContext model)
                                                in
                                                launcher.encodeFlags p.application
                                            )
                            in
                            { processName = processName, flags = processFlags }
                        )
                        newWindowManager

                newUrl =
                    Url.Builder.relative [] [ Url.Builder.string "s" serialized ]
            in
            ( { model | windowManager = newWindowManager }, Cmd.batch [ Cmd.map WindowManagerMessage cmd, Browser.Navigation.replaceUrl model.navigationKey newUrl ] )

        AppMessage pid proxiedMsg ->
            case proxiedMsg of
                LanternShell.Apps.LaunchAppMsg app flags ->
                    update (LaunchApp app flags) model

                _ ->
                    [ \m ->
                        pid
                            |> ProcessTable.lookup m.processTable
                            |> Maybe.map ProcessTable.processApp
                            |> Maybe.map
                                (\appModel ->
                                    let
                                        lanternApp =
                                            LanternShell.Apps.lanternAppFor appModel (appContext model)

                                        ( newAppModel, cmd ) =
                                            lanternApp.update proxiedMsg appModel
                                    in
                                    ( { m | processTable = ProcessTable.mapProcess (always newAppModel) pid m.processTable }
                                    , Cmd.map (wrapAppMessage pid) cmd
                                    )
                                )
                            |> Maybe.withDefault ( m, Cmd.none )
                    , refreshLiveQueries pid
                    , fireLiveQueries
                    ]
                        |> threadModel model

        UpdateViewport x y ->
            ( { model | viewport = { width = x, height = y } }, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )


launchApp : LanternShell.Apps.LauncherEntry Msg -> Maybe (LanternShell.Apps.Flags Msg) -> Model -> ( Model, Cmd Msg )
launchApp launcher flags model =
    let
        context =
            LanternShell.Apps.appContext model

        ( appModel, appCmd ) =
            launcher.init flags

        app =
            LanternShell.Apps.lanternAppFor appModel context

        newProcessTable =
            ProcessTable.launch appModel
                { name = app.name
                , arguments = []
                }
                model.processTable

        pid =
            newProcessTable.pid
    in
    [ \_ ->
        ( { model
            | processTable = newProcessTable
          }
        , Cmd.map (wrapAppMessage pid) appCmd
        )
    , refreshLiveQueries pid
    , update (WindowManagerMessage (LanternUi.WindowManager.SyncProcesses (ProcessTable.userPids newProcessTable)))
    ]
        |> threadModel model


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


renderApp :
    Model
    -> LanternUi.WindowManager.RenderOptions
    -> ProcessTable.Process (LanternShell.Apps.App Msg)
    -> Element Msg
renderApp model { focused, id, tabindex } process =
    let
        app =
            ProcessTable.processApp process

        content =
            (LanternShell.Apps.lanternAppFor app (LanternShell.Apps.appContext model)).view () app

        border =
            if focused then
                [ Element.Border.color model.theme.borderHighlight ]

            else
                [ LanternUi.noneAttribute ]
    in
    LanternUi.panel model.theme
        border
        { content = content
        , header =
            Just
                (LanternUi.textPanelHeader
                    [ Element.htmlAttribute (Html.Attributes.id id)
                    , Element.htmlAttribute (Html.Attributes.tabindex tabindex)
                    ]
                    process.name
                    [ LanternUi.Input.button model.theme [ Element.alignRight ] { onPress = Just <| Lantern.App.Close, label = Element.text "⨯" } ]
                )
        }
        |> Element.map (wrapAppMessage process.pid)


tools : Model -> Element Msg
tools model =
    let
        wrapRender : LanternUi.WindowManager.RenderOptions -> Element Msg
        wrapRender ({ pid } as renderOptions) =
            pid
                |> ProcessTable.lookup model.processTable
                |> Maybe.map (renderApp model renderOptions)
                |> Maybe.withDefault Element.none
    in
    LanternUi.WindowManager.render { spacing = 5, padding = 0 } wrapRender WindowManagerMessage model.windowManager


renderAppLauncher : Model -> Element Msg
renderAppLauncher model =
    ProcessTable.lookup model.processTable 0
        |> Maybe.map ProcessTable.processApp
        |> Maybe.map (\app -> (LanternShell.Apps.lanternAppFor app (LanternShell.Apps.appContext model)).view () app)
        |> Maybe.map (Element.map (wrapAppMessage 0))
        |> Maybe.withDefault Element.none


view : Model -> Browser.Document Msg
view model =
    { title = "Lantern Shell"
    , body =
        [ LanternUi.columnLayout
            model.theme
            [ Element.Font.color model.theme.fontDefault
            , Element.Font.size 15
            , Element.Font.family
                [ Element.Font.typeface "SF Mono"
                , Element.Font.typeface "Menlo"
                , Element.Font.typeface "Andale Mono"
                , Element.Font.typeface "Monaco"
                , Element.Font.monospace
                ]
            , Element.height (Element.fill |> Element.maximum model.viewport.height)
            ]
            [ renderAppLauncher model
            , Element.el
                [ Element.paddingEach { top = 0, bottom = 5, left = 5, right = 5 }
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                ]
                (tools model)
            ]
            |> Element.layout
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Background.color model.theme.bgDefault
                ]
        ]
    }
