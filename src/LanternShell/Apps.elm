module LanternShell.Apps exposing
    ( App
    , Context
    , Flags
    , LauncherEntry
    , Message(..)
    , all
    , appContext
    , appId
    , appLauncher
    , lanternAppFor
    , launcherForId
    , scripts
    )

import Lantern
import Lantern.App
import LanternShell.Apps.AppLauncher as AppLauncherApp
import LanternShell.Apps.DatabaseExplorer as DatabaseExplorerApp
import LanternShell.Apps.LogViewer as LogViewerApp
import LanternShell.Apps.Migrations as MigrationsApp
import LanternShell.Apps.ReaderQuery as ReaderQueryApp
import LanternShell.Apps.Scripts as ScriptsApp
import LanternShell.Apps.ValueInspector as ValueInspectorApp
import LanternShell.Apps.WriterQuery as WriterQueryApp
import LanternUi.Theme


type alias Context msg =
    { theme : LanternUi.Theme.Theme
    , lanternConnection : Lantern.Connection msg
    }


type alias LauncherEntry msg =
    Lantern.App.App () (Flags msg) (App msg) (Message msg)


type App msg
    = AppLauncherApp (AppLauncherApp.Model (LauncherEntry msg))
    | DatabaseExplorerApp DatabaseExplorerApp.Model
    | LogViewerApp LogViewerApp.Model
    | MigrationsApp MigrationsApp.Model
    | ReaderQueryApp ReaderQueryApp.Model
    | ScriptsApp ScriptsApp.Model
    | ValueInspectorApp (ValueInspectorApp.Model ScriptsApp.MyIO)
    | WriterQueryApp WriterQueryApp.Model


type Message msg
    = AppLauncherMsg (AppLauncherApp.Message (LauncherEntry msg))
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | LogViewerMsg LogViewerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | ReaderQueryMsg ReaderQueryApp.Message
    | ScriptsMsg ScriptsApp.Message
    | ValueInspectorMsg (ValueInspectorApp.Message ScriptsApp.MyIO)
    | WriterQueryMsg WriterQueryApp.Message
    | LaunchAppMsg (Lantern.App.App () (Flags msg) (App msg) (Message msg)) (Maybe (Flags msg))


type Flags msg
    = NoFlags
    | ScriptsFlags ScriptsApp.Flags
    | AppLauncherFlags (AppLauncherApp.Flags (LauncherEntry msg))
    | ValueInspectorFlags (ValueInspectorApp.Flags ScriptsApp.MyIO)


appId : App msg -> String
appId app =
    case app of
        AppLauncherApp _ ->
            "AppLauncher"

        DatabaseExplorerApp _ ->
            "DatabaseExplorer"

        LogViewerApp _ ->
            "LogViewer"

        MigrationsApp _ ->
            "Migrations"

        ReaderQueryApp _ ->
            "ReaderQuery"

        ScriptsApp _ ->
            "Scripts"

        ValueInspectorApp _ ->
            "ValueInspector"

        WriterQueryApp _ ->
            "WriterQuery"


launcherForId : String -> Maybe (Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg))
launcherForId id =
    case id of
        "DatabaseExplorer" ->
            Just databaseExplorer

        "LogViewer" ->
            Just logViewer

        "Migrations" ->
            Just migrations

        "ReaderQuery" ->
            Just readerQuery

        "Scripts" ->
            Just scripts

        "WriterQuery" ->
            Just writerQuery

        "ValueInspector" ->
            Just valueInspector

        _ ->
            Nothing


lanternAppFor : App msg -> (Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg))
lanternAppFor app =
    case app of
        AppLauncherApp _ ->
            appLauncher

        DatabaseExplorerApp _ ->
            databaseExplorer

        LogViewerApp _ ->
            logViewer

        MigrationsApp _ ->
            migrations

        ReaderQueryApp _ ->
            readerQuery

        ScriptsApp _ ->
            scripts

        ValueInspectorApp _ ->
            valueInspector

        WriterQueryApp _ ->
            writerQuery


all : List (Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg))
all =
    [ databaseExplorer
    , readerQuery
    , scripts
    , valueInspector
    , writerQuery
    , migrations
    , logViewer
    ]


databaseExplorer : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
databaseExplorer context =
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
        , wrapFlags = always NoFlags
        , unwrapFlags = always (Just ())
        , context = \_ -> { theme = context.theme }
        }
        DatabaseExplorerApp.lanternApp


logViewer : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
logViewer context =
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
        , wrapFlags = always NoFlags
        , unwrapFlags = always (Just ())
        , context = \_ -> { theme = context.theme, log = Lantern.log context.lanternConnection }
        }
        LogViewerApp.lanternApp


migrations : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
migrations context =
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
        , wrapFlags = always NoFlags
        , unwrapFlags = always (Just ())
        , context = \_ -> { theme = context.theme }
        }
        MigrationsApp.lanternApp


readerQuery : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
readerQuery context =
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
        , wrapFlags = always NoFlags
        , unwrapFlags = always (Just ())
        , context = \_ -> { theme = context.theme }
        }
        ReaderQueryApp.lanternApp


scripts : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
scripts context =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    ScriptsMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    ScriptsApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = ScriptsApp
        , wrapMsg =
            \msg ->
                case msg of
                    ScriptsApp.InspectValue v ->
                        LaunchAppMsg (valueInspector context) (Just <| ValueInspectorFlags { value = v })

                    otherMsg ->
                        ScriptsMsg otherMsg
        , wrapFlags = ScriptsFlags
        , unwrapFlags =
            \flags ->
                case flags of
                    ScriptsFlags f ->
                        Just f

                    _ ->
                        Nothing
        , context = \_ -> { theme = context.theme }
        }
        ScriptsApp.lanternApp


appLauncher : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
appLauncher context =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    AppLauncherMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    AppLauncherApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = AppLauncherApp
        , wrapMsg =
            \msg ->
                case msg of
                    AppLauncherApp.LaunchApp (AppLauncherApp.NativeApp v) ->
                        LaunchAppMsg v Nothing

                    AppLauncherApp.LaunchApp (AppLauncherApp.UserScript ( id, _ )) ->
                        LaunchAppMsg (scripts context) (Just <| ScriptsFlags (ScriptsApp.RunnerMode id))

                    _ ->
                        AppLauncherMsg msg
        , unwrapFlags =
            \flags ->
                case flags of
                    AppLauncherFlags _ ->
                        Just <|
                            { nativeApps =
                                all
                                    |> List.map
                                        (\launcher ->
                                            let
                                                app =
                                                    launcher context
                                            in
                                            ( app.name, AppLauncherApp.NativeApp app )
                                        )
                            }

                    _ ->
                        Nothing
        , wrapFlags = always NoFlags
        , context = \_ -> { theme = context.theme }
        }
        AppLauncherApp.lanternApp


writerQuery : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
writerQuery context =
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
        , wrapFlags = always NoFlags
        , unwrapFlags = always (Just ())
        , context = \_ -> { theme = context.theme }
        }
        WriterQueryApp.lanternApp


valueInspector : Context msg -> Lantern.App.App () (Flags msg) (App msg) (Message msg)
valueInspector context =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    ValueInspectorMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    ValueInspectorApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = ValueInspectorApp
        , wrapMsg = ValueInspectorMsg
        , wrapFlags = ValueInspectorFlags
        , unwrapFlags =
            \flags ->
                case flags of
                    ValueInspectorFlags unwrappedFlags ->
                        Just unwrappedFlags

                    _ ->
                        Nothing
        , context = \_ -> { theme = context.theme }
        }
        ValueInspectorApp.lanternApp


appContext : { a | theme : LanternUi.Theme.Theme, lanternConnection : Lantern.Connection msg } -> Context msg
appContext { theme, lanternConnection } =
    { theme = theme, lanternConnection = lanternConnection }
