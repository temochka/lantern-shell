module LanternShell.Apps exposing
    ( App
    , Context
    , Message
    , databaseExplorer
    , echo
    , lanternAppFor
    , logViewer
    , migrations
    , readerQuery
    , writerQuery
    )

import Lantern
import Lantern.App
import LanternShell.Apps.DatabaseExplorer as DatabaseExplorerApp
import LanternShell.Apps.Echo as EchoApp
import LanternShell.Apps.LogViewer as LogViewerApp
import LanternShell.Apps.Migrations as MigrationsApp
import LanternShell.Apps.ReaderQuery as ReaderQueryApp
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
    | WriterQueryApp WriterQueryApp.Model


type Message
    = EchoMsg EchoApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | LogViewerMsg LogViewerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | ReaderQueryMsg ReaderQueryApp.Message
    | WriterQueryMsg WriterQueryApp.Message


lanternAppFor : App -> (Context msg -> Lantern.App.App () App Message)
lanternAppFor app =
    case app of
        EchoApp _ ->
            echo

        DatabaseExplorerApp _ ->
            databaseExplorer

        LogViewerApp _ ->
            logViewer

        MigrationsApp _ ->
            migrations

        ReaderQueryApp _ ->
            readerQuery

        WriterQueryApp _ ->
            writerQuery


databaseExplorer : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme }
        }
        DatabaseExplorerApp.lanternApp


echo : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme }
        }
        EchoApp.lanternApp


logViewer : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme, log = Lantern.log context.lanternConnection }
        }
        LogViewerApp.lanternApp


migrations : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme }
        }
        MigrationsApp.lanternApp


readerQuery : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme }
        }
        ReaderQueryApp.lanternApp


writerQuery : Context msg -> Lantern.App.App () App Message
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
        , context = \_ -> { theme = context.theme }
        }
        WriterQueryApp.lanternApp
