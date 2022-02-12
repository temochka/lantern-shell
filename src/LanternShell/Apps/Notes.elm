module LanternShell.Apps.Notes exposing (Message, Model, init, lanternApp, update, view)

import Dict
import Element
import Element.Font
import Element.Input
import Json.Decode
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternUi.Theme
import Markdown
import Time


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias NoteId =
    Int


type alias Note =
    { id : NoteId
    , title : String
    , body : String
    }


type Resource a
    = Loading
    | Loaded a


type Model
    = List (Resource (List Note))
    | Editor NoteId String
    | Viewer Note


type Message
    = UpdateNotes (Result Lantern.Error (List Note))
    | UpdateNote (Result Lantern.Error (List Note))
    | UpdateEditorNote String
    | NewNote
    | NoteCreated (Result Lantern.Error Lantern.Query.WriterResult)
    | SwitchUi Model
    | SaveNote
    | Nop


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( List Loading
    , Cmd.none
    )


noteDecoder : Json.Decode.Decoder Note
noteDecoder =
    Json.Decode.map3
        (\id title body -> { id = id, title = title, body = body })
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "body" Json.Decode.string)


liveQueries : Model -> List (LiveQuery Message)
liveQueries model =
    case model of
        List _ ->
            [ Lantern.LiveQuery.prepare ( Lantern.Query.Query "SELECT * FROM notes ORDER BY createdAt DESC LIMIT 10" Dict.empty, noteDecoder ) UpdateNotes ]

        Editor _ _ ->
            []

        Viewer { id } ->
            [ Lantern.LiveQuery.prepare ( Lantern.Query.withArguments "SELECT * FROM notes WHERE id=$id LIMIT 1" [ ( "$id", Lantern.Query.Int id ) ], noteDecoder ) UpdateNote ]


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.app
        { name = "Notes"
        , init = \_ -> init
        , view = view
        , update = update
        , liveQueries = Just liveQueries
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub (Lantern.App.Message Message)
subscriptions model =
    case model of
        Editor _ _ ->
            Time.every 2000 (\_ -> Lantern.App.Message SaveNote)

        _ ->
            Sub.none


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        UpdateNotes result ->
            case model of
                List _ ->
                    case result of
                        Err err ->
                            Debug.todo "implement error handling"

                        Ok notes ->
                            ( List (Loaded notes), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateNote result ->
            case model of
                Viewer _ ->
                    case result of
                        Err err ->
                            let
                                _ =
                                    Debug.log "implement error handling" err
                            in
                            ( model, Cmd.none )

                        Ok [ note ] ->
                            ( Viewer note, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEditorNote body ->
            case model of
                Editor id _ ->
                    ( Editor id body, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewNote ->
            ( model
            , Lantern.writerQuery
                (Lantern.Query.withArguments
                    "INSERT INTO notes (title, body, createdAt, updatedAt) VALUES ($title, $body, datetime('now'), datetime('now'))"
                    [ ( "$title", Lantern.Query.String "" ), ( "$body", Lantern.Query.String "" ) ]
                )
                NoteCreated
                |> Lantern.App.call
            )

        NoteCreated result ->
            case result of
                Ok { lastInsertRowId } ->
                    ( Editor lastInsertRowId ""
                    , Cmd.none
                    )

                Err _ ->
                    Debug.todo "Not implemented"

        SaveNote ->
            case model of
                Editor id body ->
                    ( model
                    , Lantern.writerQuery
                        (Lantern.Query.withArguments
                            "UPDATE notes SET body=$body, updatedAt=datetime('now') WHERE id=$id"
                            [ ( "$body", Lantern.Query.String body ), ( "$id", Lantern.Query.Int id ) ]
                        )
                        (\_ -> Nop)
                        |> Lantern.App.call
                    )

                _ ->
                    ( model, Cmd.none )

        SwitchUi newModel ->
            ( newModel, Cmd.none )

        Nop ->
            ( model, Cmd.none )


view : Context -> Model -> Element.Element (Lantern.App.Message Message)
view _ model =
    Element.row
        [ Element.width Element.fill, Element.height Element.fill ]
        [ Element.column [ Element.width (Element.px 120), Element.height Element.fill ]
            [ Element.Input.button [ Element.width Element.fill ]
                { onPress = Just (NewNote |> Lantern.App.Message), label = Element.text "New" }
            , Element.Input.button [ Element.width Element.fill ]
                { onPress = Just (SwitchUi (List Loading) |> Lantern.App.Message), label = Element.text "List" }
            ]
        , case model of
            List notesResource ->
                case notesResource of
                    Loading ->
                        Element.text "loading..."

                    Loaded notes ->
                        notes
                            |> List.map
                                (\({ body } as note) ->
                                    Element.Input.button
                                        []
                                        { onPress = Just (SwitchUi (Viewer note) |> Lantern.App.Message)
                                        , label =
                                            Element.row
                                                []
                                                [ Element.el
                                                    [ Element.alignTop
                                                    , Element.onLeft
                                                        (Element.el
                                                            [ Element.rotate (-pi / 2)
                                                            , Element.centerY
                                                            , Element.height (Element.px 10)
                                                            , Element.Font.variant Element.Font.smallCaps
                                                            ]
                                                            (Element.text "wed")
                                                        )
                                                    ]
                                                    (Element.el [ Element.Font.size 50, Element.Font.bold ] (Element.text "17"))
                                                , Element.el
                                                    [ Element.width Element.fill, Element.alignTop ]
                                                    (Element.html (Markdown.toHtml [] body))
                                                ]
                                        }
                                )
                            |> Element.column
                                [ Element.width Element.fill
                                , Element.height Element.fill
                                ]

            Editor _ body ->
                Element.Input.multiline
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.alignTop
                    ]
                    { label = Element.Input.labelHidden "Text"
                    , onChange = UpdateEditorNote >> Lantern.App.Message
                    , text = body
                    , placeholder = Just (Element.Input.placeholder [] (Element.text "Type something great!"))
                    , spellcheck = True
                    }

            Viewer { id, body } ->
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ Element.row [ Element.width Element.fill ]
                        [ Element.Input.button [ Element.alignRight ] { onPress = Just (Lantern.App.Message (SwitchUi (Editor id body))), label = Element.text "Edit" } ]
                    , Element.html (Markdown.toHtml [] body)
                    ]
        ]
