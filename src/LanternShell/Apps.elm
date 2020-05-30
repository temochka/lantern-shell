module LanternShell.Apps exposing
    ( App
    , Context
    , Message
    , all
    , appId
    , databaseExplorer
    , echo
    , flashcardGenerator
    , lanternAppFor
    , launcherForId
    , logViewer
    , migrations
    , readerQuery
    , writerQuery
    )

import Lantern
import Lantern.App
import LanternShell.Apps.DatabaseExplorer as DatabaseExplorerApp
import LanternShell.Apps.Echo as EchoApp
import LanternShell.Apps.FlashcardGenerator as FlashcardGeneratorApp
import LanternShell.Apps.LogViewer as LogViewerApp
import LanternShell.Apps.Migrations as MigrationsApp
import LanternShell.Apps.Notes as NotesApp
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
    | FlashcardGeneratorApp FlashcardGeneratorApp.Model
    | LogViewerApp LogViewerApp.Model
    | MigrationsApp MigrationsApp.Model
    | NotesApp NotesApp.Model
    | ReaderQueryApp ReaderQueryApp.Model
    | WriterQueryApp WriterQueryApp.Model


type Message
    = EchoMsg EchoApp.Message
    | DatabaseExplorerMsg DatabaseExplorerApp.Message
    | FlashcardGeneratorMsg FlashcardGeneratorApp.Message
    | LogViewerMsg LogViewerApp.Message
    | MigrationsMsg MigrationsApp.Message
    | NotesMsg NotesApp.Message
    | ReaderQueryMsg ReaderQueryApp.Message
    | WriterQueryMsg WriterQueryApp.Message


appId : App -> String
appId app =
    case app of
        DatabaseExplorerApp _ ->
            "DatabaseExplorer"

        EchoApp _ ->
            "Echo"

        FlashcardGeneratorApp _ ->
            "FlashcardGenerator"

        LogViewerApp _ ->
            "LogViewer"

        MigrationsApp _ ->
            "Migrations"

        NotesApp _ ->
            "Notes"

        ReaderQueryApp _ ->
            "ReaderQuery"

        WriterQueryApp _ ->
            "WriterQuery"


launcherForId : String -> Maybe (Context msg -> Lantern.App.App () App Message)
launcherForId id =
    case id of
        "DatabaseExplorer" ->
            Just databaseExplorer

        "Echo" ->
            Just echo

        "FlashcardGenerator" ->
            Just flashcardGenerator

        "LogViewer" ->
            Just logViewer

        "Migrations" ->
            Just migrations

        "Notes" ->
            Just notes

        "ReaderQuery" ->
            Just readerQuery

        "WriterQuery" ->
            Just writerQuery

        _ ->
            Nothing


lanternAppFor : App -> (Context msg -> Lantern.App.App () App Message)
lanternAppFor app =
    case app of
        DatabaseExplorerApp _ ->
            databaseExplorer

        EchoApp _ ->
            echo

        FlashcardGeneratorApp _ ->
            flashcardGenerator

        LogViewerApp _ ->
            logViewer

        MigrationsApp _ ->
            migrations

        NotesApp _ ->
            notes

        ReaderQueryApp _ ->
            readerQuery

        WriterQueryApp _ ->
            writerQuery


all : List (Context msg -> Lantern.App.App () App Message)
all =
    [ echo
    , databaseExplorer
    , flashcardGenerator
    , notes
    , readerQuery
    , writerQuery
    , migrations
    , logViewer
    ]


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


flashcardGenerator : Context msg -> Lantern.App.App () App Message
flashcardGenerator context =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    FlashcardGeneratorMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    FlashcardGeneratorApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = FlashcardGeneratorApp
        , wrapMsg = FlashcardGeneratorMsg
        , context = \_ -> { theme = context.theme }
        }
        FlashcardGeneratorApp.lanternApp


notes : Context msg -> Lantern.App.App () App Message
notes context =
    Lantern.App.mount
        { unwrapMsg =
            \wrappedMsg ->
                case wrappedMsg of
                    NotesMsg unwrappedMsg ->
                        Just unwrappedMsg

                    _ ->
                        Nothing
        , unwrapModel =
            \wrappedModel ->
                case wrappedModel of
                    NotesApp unwrappedModel ->
                        Just unwrappedModel

                    _ ->
                        Nothing
        , wrapModel = NotesApp
        , wrapMsg = NotesMsg
        , context = \_ -> { theme = context.theme }
        }
        NotesApp.lanternApp


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
