module LanternShell.Apps exposing
    ( App
    , Context
    , Message(..)
    , all
    , appId
    , lanternAppFor
    , launcherForId
    , scripts
    )

import Lantern
import Lantern.App
import LanternShell.Apps.DatabaseExplorer as DatabaseExplorerApp
import LanternShell.Apps.Echo as EchoApp
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


type App
    = EchoApp EchoApp.Model
    | DatabaseExplorerApp DatabaseExplorerApp.Model
    | LogViewerApp LogViewerApp.Model
    | MigrationsApp MigrationsApp.Model
    | ReaderQueryApp ReaderQueryApp.Model
    | ScriptsApp ScriptsApp.Model
    | ValueInspectorApp ValueInspectorApp.Model
    | WriterQueryApp WriterQueryApp.Model


type Message
    = EchoMsg EchoApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | LogViewerMsg LogViewerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | ReaderQueryMsg ReaderQueryApp.Message
    | ScriptsMsg ScriptsApp.Message
    | ValueInspectorMsg ValueInspectorApp.Message
    | WriterQueryMsg WriterQueryApp.Message
    | LaunchAppMsg (Lantern.App.App () () App Message)


appId : App -> String
appId app =
    case app of
        DatabaseExplorerApp _ ->
            "DatabaseExplorer"

        EchoApp _ ->
            "Echo"

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


launcherForId : String -> Maybe (Context msg -> Lantern.App.App () () App Message)
launcherForId id =
    case id of
        "DatabaseExplorer" ->
            Just databaseExplorer

        "Echo" ->
            Just echo

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
            Just (valueInspector Nothing)

        _ ->
            Nothing


lanternAppFor : App -> (Context msg -> Lantern.App.App () () App Message)
lanternAppFor app =
    case app of
        DatabaseExplorerApp _ ->
            databaseExplorer

        EchoApp _ ->
            echo

        LogViewerApp _ ->
            logViewer

        MigrationsApp _ ->
            migrations

        ReaderQueryApp _ ->
            readerQuery

        ScriptsApp _ ->
            scripts

        ValueInspectorApp _ ->
            valueInspector Nothing

        WriterQueryApp _ ->
            writerQuery


all : List (Context msg -> Lantern.App.App () () App Message)
all =
    [ echo
    , databaseExplorer
    , readerQuery
    , scripts
    , valueInspector Nothing
    , writerQuery
    , migrations
    , logViewer
    ]


databaseExplorer : Context msg -> Lantern.App.App () () App Message
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        DatabaseExplorerApp.lanternApp


echo : Context msg -> Lantern.App.App () () App Message
echo context =
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        EchoApp.lanternApp


logViewer : Context msg -> Lantern.App.App () () App Message
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme, log = Lantern.log context.lanternConnection }
        }
        LogViewerApp.lanternApp


migrations : Context msg -> Lantern.App.App () () App Message
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        MigrationsApp.lanternApp


readerQuery : Context msg -> Lantern.App.App () () App Message
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        ReaderQueryApp.lanternApp


scripts : Context msg -> Lantern.App.App () () App Message
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
                        LaunchAppMsg (valueInspector (Just { value = v }) context)

                    otherMsg ->
                        ScriptsMsg otherMsg
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        ScriptsApp.lanternApp


writerQuery : Context msg -> Lantern.App.App () () App Message
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
        , flags = Nothing
        , context = \_ -> { theme = context.theme }
        }
        WriterQueryApp.lanternApp


valueInspector : Maybe ValueInspectorApp.Flags -> Context msg -> Lantern.App.App () () App Message
valueInspector flags context =
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
        , flags = flags
        , context = \_ -> { theme = context.theme }
        }
        ValueInspectorApp.lanternApp
