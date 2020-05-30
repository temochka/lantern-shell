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
import Element.Input
import Html.Attributes
import Json.Decode
import Keyboard.Event
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery(..))
import LanternShell.Apps
import LanternUi
import LanternUi.FuzzySelect
import LanternUi.Theme exposing (lightTheme)
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


type alias LauncherEntry =
    LanternShell.Apps.Context Msg -> ( LanternShell.Apps.App, Cmd (Lantern.App.Message LanternShell.Apps.Message) )


type alias Model =
    { lanternConnection : Lantern.Connection Msg
    , processTable : ProcessTable LanternShell.Apps.App
    , windowManager : LanternUi.WindowManager.WindowManager
    , appLauncherQuery : String
    , fuzzySelect : LanternUi.FuzzySelect.FuzzySelect
    , theme : LanternUi.Theme.Theme
    , liveQueriesCache : Dict ProcessTable.Pid (List (LiveQuery Msg))
    , navigationKey : Browser.Navigation.Key
    }


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        lanternConnection =
            Lantern.newConnection lanternRequestPort

        initialTheme =
            LanternUi.Theme.lightTheme

        ( windowManager, launchers ) =
            url
                |> (Url.Parser.Query.string "s" |> Url.Parser.query |> Url.Parser.parse)
                |> Maybe.andThen identity
                |> Maybe.andThen (LanternUi.WindowManager.deserialize LanternShell.Apps.launcherForId)
                |> Maybe.withDefault ( LanternUi.WindowManager.new [], [] )

        starterModel =
            { lanternConnection = lanternConnection
            , windowManager = windowManager
            , processTable = ProcessTable.empty
            , fuzzySelect = Nothing
            , theme = initialTheme
            , liveQueriesCache = Dict.empty
            , appLauncherQuery = ""
            , navigationKey = key
            }
    in
    launchers
        |> List.foldl
            (\launcher ( model, cmd ) -> update (LaunchApp (launcher >> .init)) model |> Tuple.mapSecond (\newCmd -> Cmd.batch [ cmd, newCmd ]))
            ( starterModel
            , Cmd.none
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
                        (LanternShell.Apps.lanternAppFor appModel (appContext model)).subscriptions appModel
                            |> Sub.map (wrapAppMessage appProcess.pid)
                    )
                |> Sub.batch
    in
    Sub.batch
        [ Lantern.subscriptions LanternMessage lanternResponsePort
        , Browser.Events.onKeyDown (handleShortcuts model)
        , appSubs
        ]



-- UPDATE


type Msg
    = Nop
    | AppLauncherMessage LanternUi.FuzzySelect.Message
    | AppMessage ProcessTable.Pid LanternShell.Apps.Message
    | CloseApp ProcessTable.Pid
    | FocusAppLauncher
    | LanternMessage (Lantern.Message Msg)
    | LaunchApp LauncherEntry
    | WindowManagerMessage LanternUi.WindowManager.Message
    | UpdateLauncherQuery String
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest


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


refreshLiveQueries : ProcessTable.Pid -> Model -> ( Model, Cmd Msg )
refreshLiveQueries pid model =
    pid
        |> ProcessTable.lookup model.processTable
        |> Maybe.map ProcessTable.processApp
        |> Maybe.map
            (\appModel ->
                let
                    lanternApp =
                        LanternShell.Apps.lanternAppFor appModel (appContext model)

                    newLiveQueries =
                        lanternApp.liveQueries appModel |> List.map (Lantern.LiveQuery.map (AppMessage pid))

                    liveQueriesCache =
                        Dict.insert pid newLiveQueries model.liveQueriesCache

                    liveQueriesChanged =
                        Dict.get pid model.liveQueriesCache
                            |> Maybe.map (Lantern.LiveQuery.areEqualLists newLiveQueries >> not)
                            |> Maybe.withDefault True

                    cmd =
                        if liveQueriesChanged then
                            Lantern.liveQueries (liveQueriesCache |> Dict.values |> List.concatMap identity)
                                |> Cmd.map LanternMessage

                        else
                            Cmd.none
                in
                ( { model | liveQueriesCache = liveQueriesCache }, cmd )
            )
        |> Maybe.withDefault ( model, Cmd.none )


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
            ( { model | fuzzySelect = LanternUi.FuzzySelect.update proxiedMsg model.fuzzySelect }, Cmd.none )

        LaunchApp launcher ->
            let
                context =
                    appContext model

                ( appModel, appCmd ) =
                    launcher context

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
                    , fuzzySelect = Nothing
                    , appLauncherQuery = ""
                  }
                , Cmd.map (wrapAppMessage pid) appCmd
                )
            , refreshLiveQueries pid
            , update (WindowManagerMessage (LanternUi.WindowManager.SyncProcesses (ProcessTable.pids newProcessTable)))
            ]
                |> threadModel model

        WindowManagerMessage proxiedMsg ->
            let
                ( newWindowManager, cmd ) =
                    LanternUi.WindowManager.update proxiedMsg model.windowManager

                serialized =
                    LanternUi.WindowManager.serialize
                        (ProcessTable.lookup model.processTable
                            >> Maybe.map (ProcessTable.processApp >> LanternShell.Apps.appId)
                            >> Maybe.withDefault ""
                        )
                        newWindowManager

                newUrl =
                    Url.Builder.relative [] [ Url.Builder.string "s" serialized ]
            in
            ( { model | windowManager = newWindowManager }, Cmd.batch [ Cmd.map WindowManagerMessage cmd, Browser.Navigation.replaceUrl model.navigationKey newUrl ] )

        AppMessage pid proxiedMsg ->
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
            ]
                |> threadModel model

        UpdateLauncherQuery query ->
            ( { model | appLauncherQuery = query }, Cmd.none )

        UrlChange _ ->
            ( model, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal _ ->
                    ( model, Cmd.none )

                Browser.External url ->
                    ( model, Browser.Navigation.load url )


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
    -> ProcessTable.Process LanternShell.Apps.App
    -> Element Msg
renderApp model { focused, id, tabindex } process =
    let
        app =
            ProcessTable.processApp process

        content =
            (LanternShell.Apps.lanternAppFor app (appContext model)).view () app

        border =
            if focused then
                [ Element.Border.shadow { offset = ( 0, 0 ), size = 2, blur = 0, color = model.theme.borderHighlight } ]

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
    Element.row
        [ Element.width Element.fill, Element.spacing 10, Element.Background.color model.theme.bgContrast, Element.paddingXY 25 10 ]
        [ Element.el [ Element.Font.color model.theme.fontContrastInactive ] (Element.text ">")
        , LanternUi.FuzzySelect.fuzzySelect
            lightTheme
            { options = LanternShell.Apps.all |> List.map (\app -> ( (app (appContext model)).name, app >> .init ))
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Launch app"
            , id = Just "lanternAppLauncher"
            , onInternalMessage = AppLauncherMessage
            , onOptionSelect = LaunchApp
            , onQueryChange = UpdateLauncherQuery
            , state = model.fuzzySelect
            , query = model.appLauncherQuery
            }
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Lantern Shell"
    , body =
        [ LanternUi.columnLayout
            model.theme
            []
            [ renderAppLauncher model, Element.el [ Element.paddingXY 20 5, Element.width Element.fill, Element.height Element.fill ] (tools model) ]
            |> Element.layout [ Element.width Element.fill, Element.Background.color model.theme.bgDefault ]
        ]
    }
