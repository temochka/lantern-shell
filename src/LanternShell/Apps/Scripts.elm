module LanternShell.Apps.Scripts exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Cell(..), Env, Exception(..), IO(..), InputCell(..), InputKey, TextFormat(..), Thunk(..), UI, Value(..))
import Enclojure.ValueMap
import File.Download
import Html.Events
import Json.Decode
import Keyboard.Event
import Keyboard.Key exposing (Key(..))
import Lantern
import Lantern.App
import Lantern.Http
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternUi
import LanternUi.FuzzySelect exposing (FuzzySelect)
import LanternUi.Input
import LanternUi.Theme
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type ConsoleEntry
    = ConsoleString String
    | UiTrace UiModel
    | Savepoint_ ( ( Located IO, Env ), Maybe Thunk )
    | Success Value
    | Failure ( Located Exception, Env )


type alias Console =
    List ConsoleEntry


type alias BrowserModel =
    { scripts : List Script
    , query : String
    }


type alias EditorModel =
    { interpreter : Interpreter
    , scriptEditor : Script
    , console : Console
    , repl : String
    }


type Model
    = Browser BrowserModel
    | Editor EditorModel


stackLimit : Int
stackLimit =
    100000


printLn : String -> Console -> Console
printLn string console =
    ConsoleString string :: console


printResult : Interpreter -> Console -> Console
printResult interpreter console =
    case interpreter of
        Done ( value, _ ) ->
            Success value :: console

        UI model _ ->
            UiTrace model :: console

        Panic e ->
            Failure e :: console

        _ ->
            console


recordSavepoint : ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk ) -> Console -> Console
recordSavepoint ( ret, thunk ) console =
    ret
        |> Result.map (\val -> Savepoint_ ( val, thunk ) :: console)
        |> Result.withDefault console


scriptName : Script -> String
scriptName { name } =
    let
        trimmedName =
            String.trim name
    in
    if not <| String.isEmpty trimmedName then
        String.trim name

    else
        "unnamed script"


type Message
    = FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell
    | UpdateScripts (Result Lantern.Error (List Script))
    | HandleIO ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk )
    | Rewind ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk )
    | InspectValue Value
    | CreateScript
    | ScriptCreated (Result Lantern.Error Lantern.Query.WriterResult)
    | SaveScript
    | ScriptSaved (Result Lantern.Error Lantern.Query.WriterResult)
    | NewScript
    | DeleteScript Script
    | EditScript Script
    | UpdateName String
    | UpdateCode String
    | UpdateRepl String
    | UpdateBrowserQuery String
    | Eval String
    | DownloadFile String String String
    | NoOp


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( Browser { scripts = [], query = "" }
    , Cmd.none
    )


unsavedScript : { id : Maybe Int, code : String, name : String, input : String }
unsavedScript =
    { id = Nothing, code = "", name = "", input = "" }


type alias UiModel =
    { fuzzySelects : Dict String FuzzySelect, enclojureUi : UI }


type Interpreter
    = Stopped
    | Blocked
    | UI UiModel ( Env, Maybe Thunk )
    | Running
    | Done ( Value, Env )
    | Panic ( Located Exception, Env )


responseToValue : Lantern.Http.Response -> Value
responseToValue response =
    Map <|
        Enclojure.ValueMap.fromList
            [ ( Keyword "status", Located.fakeLoc <| Number (Enclojure.Types.Int response.status) )
            , ( Keyword "headers"
              , Located.fakeLoc <|
                    Map (response.headers |> List.map (\( k, v ) -> ( String k, Located.fakeLoc (String v) )) |> Enclojure.ValueMap.fromList)
              )
            , ( Keyword "body"
              , response.body |> Maybe.map String |> Maybe.withDefault Nil |> Located.fakeLoc
              )
            ]


trampoline : ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk ) -> Int -> ( Interpreter, Cmd (Lantern.App.Message Message) )
trampoline ( result, thunk ) maxdepth =
    case result of
        Ok ( io, env ) ->
            if maxdepth <= 0 then
                ( Panic ( Located.fakeLoc (Exception "Stack level too deep"), env ), Cmd.none )

            else
                case Located.getValue io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline (continuation (Located.replace io v) env) (maxdepth - 1)

                            Nothing ->
                                ( Done ( v, env ), Cmd.none )

                    Savepoint v ->
                        ( Running
                        , Task.perform identity (Task.succeed (Lantern.App.Message <| HandleIO ( Ok ( Located.replace io (Const v), env ), thunk )))
                        )

                    Http request ->
                        ( Running
                        , Lantern.httpRequest
                            { method = request.method
                            , headers = request.headers
                            , url = request.url
                            , body = request.body
                            , expect =
                                \response ->
                                    HandleIO ( Ok ( Located.replace io (Const (responseToValue response)), env ), thunk )
                            }
                            |> Lantern.App.call
                        )

                    Sleep ms ->
                        ( Running
                        , Process.sleep ms
                            |> Task.perform
                                (\_ ->
                                    Lantern.App.Message <| HandleIO ( Ok ( Located.replace io (Const Nil), env ), thunk )
                                )
                        )

                    ShowUI ui ->
                        ( UI { enclojureUi = ui, fuzzySelects = Dict.empty } ( env, thunk )
                        , Cmd.none
                        )

                    ReadField _ ->
                        ( Panic ( Located.fakeLoc (Exception "Not implemented"), env ), Cmd.none )

        Err e ->
            ( Panic e, Cmd.none )


updateBrowser :
    Model
    -> (BrowserModel -> ( BrowserModel, Cmd (Lantern.App.Message Message) ))
    -> ( Model, Cmd (Lantern.App.Message Message) )
updateBrowser model updateFn =
    case model of
        Browser m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Browser newM, cmd )

        _ ->
            ( model, Cmd.none )


updateEditor :
    Model
    -> (EditorModel -> ( EditorModel, Cmd (Lantern.App.Message Message) ))
    -> ( Model, Cmd (Lantern.App.Message Message) )
updateEditor model updateFn =
    case model of
        Editor m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Editor newM, cmd )

        _ ->
            ( model, Cmd.none )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg appModel =
    case msg of
        DownloadFile filename contentType content ->
            ( appModel, File.Download.string filename contentType content )

        DeleteScript script ->
            updateBrowser appModel
                (\model ->
                    script.id
                        |> Maybe.map
                            (\id ->
                                Lantern.Query.withArguments
                                    "DELETE FROM scripts WHERE id=$id"
                                    [ ( "$id", id |> Lantern.Query.Int )
                                    ]
                            )
                        |> Maybe.map (\query -> ( model, Lantern.writerQuery query ScriptSaved |> Lantern.App.call ))
                        |> Maybe.withDefault ( model, Cmd.none )
                )

        FuzzySelectMessage id selectMsg ->
            updateEditor appModel
                (\model ->
                    case model.interpreter of
                        UI ({ fuzzySelects } as ui) args ->
                            let
                                updatedFuzzySelects =
                                    Dict.update id
                                        (Maybe.withDefault LanternUi.FuzzySelect.hidden
                                            >> LanternUi.FuzzySelect.update selectMsg
                                            >> Just
                                        )
                                        fuzzySelects
                            in
                            ( { model | interpreter = UI { ui | fuzzySelects = updatedFuzzySelects } args }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )
                )

        UpdateCode code ->
            updateEditor appModel
                (\model ->
                    let
                        scriptEditor =
                            model.scriptEditor

                        editor =
                            { scriptEditor | code = code }
                    in
                    ( { model | scriptEditor = editor }, Cmd.none )
                )

        UpdateRepl code ->
            updateEditor appModel
                (\model ->
                    ( { model | repl = code }, Cmd.none )
                )

        Eval code ->
            updateEditor appModel
                (\model ->
                    let
                        mEnv =
                            case model.interpreter of
                                Done ( _, env ) ->
                                    Just env

                                Panic ( _, env ) ->
                                    Just env

                                _ ->
                                    Nothing
                    in
                    mEnv
                        |> Maybe.map
                            (\env ->
                                let
                                    ( interpreter, retMsg ) =
                                        trampoline (Enclojure.eval env code) stackLimit
                                in
                                ( { model
                                    | interpreter = interpreter
                                    , console =
                                        model.console
                                            |> printLn "Evaluating from REPL"
                                            |> printResult interpreter
                                    , repl = ""
                                  }
                                , retMsg
                                )
                            )
                        |> Maybe.withDefault
                            ( { model
                                | console =
                                    model.console
                                        |> printLn "Can't eval from REPL at this time"
                              }
                            , Cmd.none
                            )
                )

        UpdateName name ->
            updateEditor appModel
                (\model ->
                    let
                        scriptEditor =
                            model.scriptEditor

                        editor =
                            { scriptEditor | name = name }
                    in
                    ( { model | scriptEditor = editor }, Cmd.none )
                )

        Run ->
            updateEditor appModel
                (\model ->
                    let
                        ( interpreter, retMsg ) =
                            trampoline (Enclojure.eval Runtime.emptyEnv model.scriptEditor.code) stackLimit
                    in
                    ( { model
                        | interpreter = interpreter
                        , console =
                            model.console
                                |> printLn ("Starting " ++ scriptName model.scriptEditor)
                                |> printResult interpreter
                      }
                    , retMsg
                    )
                )

        Stop ->
            updateEditor appModel
                (\model ->
                    ( { model
                        | interpreter = Panic ( Located.fakeLoc (Exception "Terminated"), Runtime.emptyEnv )
                      }
                    , Cmd.none
                    )
                )

        HandleIO ret ->
            updateEditor appModel
                (\model ->
                    let
                        ( interpreter, retMsg ) =
                            trampoline ret stackLimit
                    in
                    ( { model
                        | interpreter = interpreter
                        , console =
                            model.console
                                |> recordSavepoint ret
                                |> printResult interpreter
                      }
                    , retMsg
                    )
                )

        Rewind ret ->
            let
                ( newAppModel, _ ) =
                    updateEditor appModel (\model -> ( { model | interpreter = Running }, Cmd.none ))
            in
            update (HandleIO ret) newAppModel

        UpdateInputRequest name v ->
            updateEditor
                appModel
                (\model ->
                    case model.interpreter of
                        UI ({ enclojureUi } as ui) ( env, thunk ) ->
                            case v of
                                Button _ ->
                                    let
                                        exitCode =
                                            Located.fakeLoc <| Keyword name

                                        state =
                                            Located.fakeLoc <| Enclojure.uiToValue enclojureUi

                                        console =
                                            printResult model.interpreter model.console
                                    in
                                    ( { model | interpreter = Running, console = console }
                                    , Task.perform
                                        identity
                                        (Task.succeed (Lantern.App.Message <| HandleIO ( Ok ( Located.fakeLoc (Const (List [ exitCode, state ])), env ), thunk )))
                                    )

                                _ ->
                                    let
                                        updatedInputs =
                                            Dict.insert name v ui.enclojureUi.inputs

                                        updatedUi =
                                            { enclojureUi | inputs = updatedInputs }

                                        updatedModel =
                                            { model | interpreter = UI { ui | enclojureUi = updatedUi } ( env, thunk ) }
                                    in
                                    ( updatedModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
                )

        UpdateScripts result ->
            updateBrowser appModel
                (\model ->
                    case result of
                        Ok scripts ->
                            ( { model | scripts = scripts }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )
                )

        SaveScript ->
            updateEditor appModel
                (\model ->
                    let
                        cmd =
                            model.scriptEditor.id
                                |> Maybe.map
                                    (\id ->
                                        Lantern.Query.withArguments
                                            "UPDATE scripts SET name=$name, code=$code, input=$input, updated_at=datetime('now') WHERE id=$id"
                                            [ ( "$name", model.scriptEditor.name |> Lantern.Query.String )
                                            , ( "$code", model.scriptEditor.code |> Lantern.Query.String )
                                            , ( "$input", model.scriptEditor.input |> Lantern.Query.String )
                                            , ( "$id", id |> Lantern.Query.Int )
                                            ]
                                    )
                                |> Maybe.map (\query -> Lantern.writerQuery query ScriptSaved |> Lantern.App.call)
                                |> Maybe.withDefault Cmd.none
                    in
                    ( model, cmd )
                )

        NewScript ->
            ( Editor
                { interpreter = Stopped
                , scriptEditor = unsavedScript
                , console = []
                , repl = ""
                }
            , Cmd.none
            )

        ScriptSaved _ ->
            ( appModel, Cmd.none )

        CreateScript ->
            updateEditor appModel
                (\model ->
                    let
                        query =
                            Lantern.Query.withArguments
                                "INSERT INTO scripts (name, code, input, created_at, updated_at) VALUES ($name, $code, $input, datetime('now'), datetime('now'))"
                                [ ( "$name", model.scriptEditor.name |> Lantern.Query.String )
                                , ( "$code", model.scriptEditor.code |> Lantern.Query.String )
                                , ( "$input", model.scriptEditor.input |> Lantern.Query.String )
                                ]
                    in
                    ( model, Lantern.writerQuery query ScriptCreated |> Lantern.App.call )
                )

        ScriptCreated result ->
            updateEditor appModel
                (\model ->
                    result
                        |> Result.map
                            (\writerResult ->
                                let
                                    editor =
                                        model.scriptEditor

                                    newEditor =
                                        { editor | id = Just writerResult.lastInsertRowId }
                                in
                                ( { model | scriptEditor = newEditor }, Cmd.none )
                            )
                        |> Result.withDefault ( model, Cmd.none )
                )

        EditScript script ->
            ( Editor
                { interpreter = Stopped
                , scriptEditor = script
                , console = []
                , repl = ""
                }
            , Cmd.none
            )

        -- Beware: the Apps namespace hijacks this message and launches the inspector
        -- would be nice to make this more explicit but it'll take a lot of refactoring
        InspectValue _ ->
            ( appModel, Cmd.none )

        NoOp ->
            ( appModel, Cmd.none )

        UpdateBrowserQuery query ->
            updateBrowser appModel
                (\model ->
                    ( { model | query = query }
                    , Cmd.none
                    )
                )


inspectEnv : Env -> String
inspectEnv { global, local } =
    let
        inspectDict dict =
            "{" ++ Dict.foldl (\k v s -> s ++ "\"" ++ k ++ "\" " ++ Runtime.inspect v ++ " ") "" dict ++ "}"
    in
    "{\"local\" " ++ inspectDict local ++ " \"global\" " ++ inspectDict global ++ "}"


isRunning : Interpreter -> Bool
isRunning interpreter =
    case interpreter of
        Running ->
            True

        Blocked ->
            True

        Panic _ ->
            False

        UI _ _ ->
            True

        Done _ ->
            False

        Stopped ->
            False


renderUI : Context -> UiModel -> Element (Lantern.App.Message Message)
renderUI context uiModel =
    let
        { cell, inputs } =
            uiModel.enclojureUi

        { fuzzySelects } =
            uiModel
    in
    case cell of
        VStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { cell = c, inputs = inputs } })
                |> Element.column [ Element.width Element.fill, Element.alignTop, Element.spacing 10 ]

        HStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { cell = c, inputs = inputs } })
                |> Element.row [ Element.width Element.fill ]

        Input key ->
            Dict.get key inputs
                |> Maybe.map
                    (\input ->
                        case input of
                            TextInput opts s ->
                                if List.isEmpty opts.suggestions then
                                    LanternUi.Input.multiline
                                        context.theme
                                        []
                                        { onChange = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , placeholder = Nothing
                                        , label = Element.Input.labelHidden key
                                        , text = s
                                        , spellcheck = False
                                        }

                                else
                                    LanternUi.FuzzySelect.fuzzySelect
                                        context.theme
                                        { label = Element.Input.labelHidden ""
                                        , onQueryChange = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , onInternalMessage = FuzzySelectMessage key >> Lantern.App.Message
                                        , onOptionSelect = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , options = List.map2 Tuple.pair opts.suggestions opts.suggestions
                                        , placeholder = Nothing
                                        , query = s
                                        , state = Dict.get key fuzzySelects |> Maybe.withDefault LanternUi.FuzzySelect.hidden
                                        , id = Nothing
                                        }

                            MaskedTextInput s ->
                                LanternUi.Input.password
                                    context.theme
                                    []
                                    { onChange = MaskedTextInput >> UpdateInputRequest key >> Lantern.App.Message
                                    , placeholder = Nothing
                                    , label = Element.Input.labelHidden ""
                                    , text = s
                                    , show = False
                                    }

                            Button { title } ->
                                LanternUi.Input.button
                                    context.theme
                                    []
                                    { onPress = Just (UpdateInputRequest key input |> Lantern.App.Message)
                                    , label = Element.text title
                                    }

                            Download { name, contentType, content } ->
                                LanternUi.Input.button
                                    context.theme
                                    []
                                    { onPress = Just (DownloadFile name contentType content |> Lantern.App.Message)
                                    , label = Element.text "Download"
                                    }
                    )
                |> Maybe.withDefault Element.none

        Text textCells ->
            textCells
                |> List.map
                    (\c ->
                        case c of
                            Plain t ->
                                Element.text t
                    )
                |> Element.paragraph []


activeUi : Interpreter -> Maybe UiModel
activeUi interpreter =
    case interpreter of
        UI model _ ->
            Just model

        _ ->
            Nothing


viewEditor : Context -> EditorModel -> Element (Lantern.App.Message Message)
viewEditor context model =
    let
        runButtonTitle =
            case model.interpreter of
                Stopped ->
                    "Start"

                _ ->
                    "Restart"

        runButton =
            LanternUi.Input.button context.theme [] { label = Element.text runButtonTitle, onPress = Just <| Lantern.App.Message Run }

        evalButton =
            LanternUi.Input.button context.theme [] { label = Element.text "Eval", onPress = Just <| Lantern.App.Message (Eval model.scriptEditor.code) }

        saveButton =
            if model.scriptEditor.id == Nothing then
                LanternUi.Input.button context.theme [] { label = Element.text "Save", onPress = Just <| Lantern.App.Message CreateScript }

            else
                LanternUi.Input.button context.theme [] { label = Element.text "Save", onPress = Just <| Lantern.App.Message SaveScript }

        toolbar =
            Element.row
                [ Element.width Element.fill, Element.spacing 10 ]
                [ saveButton, runButton, evalButton ]

        scriptEditor =
            Element.column
                [ Element.width Element.fill, Element.spacing 20 ]
                [ LanternUi.Input.text context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateName >> Lantern.App.Message
                    , text = model.scriptEditor.name
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove [] (Element.text "Script name")
                    }
                , LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    ]
                    { onChange = UpdateCode >> Lantern.App.Message
                    , value = model.scriptEditor.code
                    , language = LanternUi.Input.Enclojure
                    , label = Just <| Element.text "Code"
                    }
                ]

        consoleWithActiveUi =
            activeUi model.interpreter
                |> Maybe.map (\ui -> UiTrace ui :: model.console)
                |> Maybe.withDefault model.console
                |> List.take 20

        console =
            consoleWithActiveUi
                |> List.indexedMap
                    (\i entry ->
                        let
                            valueRow val =
                                Element.paragraph
                                    [ Element.width Element.fill ]
                                    [ Element.text (Runtime.inspect val)
                                    , LanternUi.Input.button context.theme
                                        []
                                        { onPress = Just (Lantern.App.Message <| InspectValue val)
                                        , label = Element.text "Inspect"
                                        }
                                    ]
                        in
                        Element.el
                            [ Element.width Element.fill
                            , if i == 0 then
                                Element.alpha 1.0

                              else
                                Element.alpha 0.6
                            ]
                            (case entry of
                                ConsoleString s ->
                                    Element.paragraph [ Element.width Element.fill ] [ Element.text s ]

                                Savepoint_ ( ( Located loc io, env ), thunk ) ->
                                    let
                                        newEnv =
                                            case model.interpreter of
                                                Done ( _, cEnv ) ->
                                                    { cEnv | local = env.local }

                                                Panic ( _, cEnv ) ->
                                                    { cEnv | local = env.local }

                                                _ ->
                                                    env

                                        rewindButton =
                                            LanternUi.Input.button context.theme
                                                []
                                                { onPress = Just (Lantern.App.Message <| Rewind ( Ok ( Located loc io, newEnv ), thunk ))
                                                , label = Element.text "Rewind"
                                                }
                                    in
                                    Element.column
                                        [ Element.width Element.fill ]
                                        [ Element.paragraph [] [ Element.text "Savepoint", rewindButton ]
                                        , case io of
                                            Const v ->
                                                valueRow v

                                            _ ->
                                                Element.none
                                        ]

                                Success val ->
                                    Element.column
                                        [ Element.width Element.fill ]
                                        [ Element.paragraph [ Element.width Element.fill ] [ Element.text "Done" ]
                                        , valueRow val
                                        ]

                                Failure ( e, _ ) ->
                                    Element.text <| Runtime.inspectLocated (Located.map Throwable e)

                                UiTrace ui ->
                                    Element.column
                                        [ Element.width Element.fill ]
                                        [ renderUI context ui ]
                            )
                    )
                |> Element.column
                    [ Element.width Element.fill
                    , Element.spacing 10
                    ]

        handleKeyPress event =
            case ( event.keyCode, event.metaKey, event.altKey ) of
                ( Keyboard.Key.Enter, True, False ) ->
                    ( Eval model.repl |> Lantern.App.Message, True )

                _ ->
                    ( NoOp |> Lantern.App.Message, False )

        repl =
            Element.column [ Element.width Element.fill, Element.spacing 20 ]
                [ LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    , Element.htmlAttribute (Html.Events.preventDefaultOn "keydown" (Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent))
                    ]
                    { onChange = UpdateRepl >> Lantern.App.Message
                    , value = model.repl
                    , language = LanternUi.Input.Enclojure
                    , label = Just <| Element.text "REPL"
                    }
                , Element.paragraph [] [ Element.text "Console" ]
                , console
                ]
    in
    LanternUi.columnLayout
        context.theme
        [ Element.width Element.fill, Element.spacing 20 ]
        [ toolbar
        , Element.column [ Element.width (Element.fillPortion 5), Element.spacing 20 ] [ scriptEditor, repl ]
        ]


viewBrowser : Context -> BrowserModel -> Element (Lantern.App.Message Message)
viewBrowser context model =
    let
        newScriptButton =
            Element.el
                [ Element.width Element.fill ]
                (LanternUi.Input.button context.theme [] { label = Element.text "New script", onPress = Just <| Lantern.App.Message NewScript })

        search =
            LanternUi.Input.text
                context.theme
                []
                { text = model.query
                , onChange = UpdateBrowserQuery >> Lantern.App.Message
                , placeholder = Just (Element.Input.placeholder [] (Element.text "Filter scripts"))
                , label = Element.Input.labelHidden "Filter scripts"
                }

        toolbar =
            Element.row
                [ Element.spacing 10 ]
                [ search
                , newScriptButton
                ]

        filteredScripts =
            model.scripts
                |> List.filter (\s -> String.isEmpty model.query || String.contains (String.toLower model.query) (String.toLower s.name))

        scriptsList =
            filteredScripts
                |> List.map
                    (\script ->
                        Element.row
                            [ Element.width Element.fill
                            , Element.spacing 20
                            ]
                            [ Element.Input.button [ Element.mouseOver [ Element.Background.color context.theme.bgHighlight ] ]
                                { onPress = Just (Lantern.App.Message <| EditScript script)
                                , label = Element.text (scriptName script)
                                }
                            , LanternUi.Input.button context.theme
                                []
                                { onPress = Just (Lantern.App.Message <| DeleteScript script)
                                , label = Element.text "Delete"
                                }
                            ]
                    )
                |> Element.column [ Element.width (Element.fillPortion 1), Element.alignTop, Element.spacing 10 ]
    in
    LanternUi.columnLayout
        context.theme
        []
        [ toolbar
        , scriptsList
        ]


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    case model of
        Browser browser ->
            viewBrowser context browser

        Editor editor ->
            viewEditor context editor


type alias Script =
    { id : Maybe Int
    , name : String
    , code : String
    , input : String
    }


tableDecoder : Json.Decode.Decoder Script
tableDecoder =
    Json.Decode.map4
        Script
        (Json.Decode.map Just (Json.Decode.field "id" Json.Decode.int))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "input" Json.Decode.string)


liveQueries : Model -> List (LiveQuery Message)
liveQueries model =
    case model of
        Browser _ ->
            let
                tablesQuery =
                    Lantern.LiveQuery.prepare ( Lantern.Query.Query "SELECT id, input, name, code FROM scripts ORDER BY name" Dict.empty, tableDecoder ) UpdateScripts
            in
            [ tablesQuery ]

        Editor _ ->
            []


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.app
        { name = "Scripts"
        , init = \_ -> init
        , view = view
        , update = update
        , liveQueries = Just liveQueries
        , subscriptions = always Sub.none
        }
