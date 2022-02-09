module LanternShell.Apps.Scripts exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Cell(..), Env, Exception(..), IO(..), InputCell(..), InputKey, TextFormat(..), Thunk(..), UI, Value(..))
import Enclojure.ValueMap
import Html.Attributes
import Html.Events
import Http
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
    | Breakpoint ( ( Located IO, Env ), Maybe Thunk )
    | Success Value
    | Failure ( Located Exception, Env )


type alias Console =
    List ConsoleEntry


type alias Model =
    { interpreter : Interpreter
    , scripts : List Script
    , scriptEditor : Script
    , console : Console
    , repl : String
    }


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


recordBreakpoint : ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk ) -> Console -> Console
recordBreakpoint ( ret, thunk ) console =
    ret
        |> Result.map (\val -> Breakpoint ( val, thunk ) :: console)
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
    | EditScript Script
    | UpdateName String
    | UpdateCode String
    | UpdateScriptInput String
    | UpdateRepl String
    | Eval String
    | NoOp


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( { interpreter = Stopped
      , scripts = []
      , scriptEditor = unsavedScript
      , console = []
      , repl = ""
      }
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


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        FuzzySelectMessage id selectMsg ->
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
                    ( { model | interpreter = UI { ui | fuzzySelects = updatedFuzzySelects } args }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateCode code ->
            let
                scriptEditor =
                    model.scriptEditor

                editor =
                    { scriptEditor | code = code }
            in
            ( { model | scriptEditor = editor }, Cmd.none )

        UpdateRepl code ->
            ( { model | repl = code }, Cmd.none )

        Eval code ->
            case model.interpreter of
                Done ( _, env ) ->
                    let
                        ( interpreter, retMsg ) =
                            trampoline
                                (Enclojure.eval
                                    (env |> Runtime.setGlobalEnv "*input*" (String model.scriptEditor.input))
                                    code
                                )
                                10000
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

                _ ->
                    ( { model | console = model.console |> printLn "Can't eval from REPL at this time" }, Cmd.none )

        UpdateName name ->
            let
                scriptEditor =
                    model.scriptEditor

                editor =
                    { scriptEditor | name = name }
            in
            ( { model | scriptEditor = editor }, Cmd.none )

        Run ->
            let
                ( interpreter, retMsg ) =
                    trampoline
                        (Enclojure.eval (Runtime.emptyEnv |> Runtime.setGlobalEnv "*input*" (String model.scriptEditor.input))
                            model.scriptEditor.code
                        )
                        10000
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

        Stop ->
            ( { model | interpreter = Panic ( Located.fakeLoc (Exception "Terminated"), Runtime.emptyEnv ) }, Cmd.none )

        HandleIO ret ->
            let
                ( interpreter, retMsg ) =
                    trampoline ret 10000
            in
            ( { model
                | interpreter = interpreter
                , console =
                    model.console
                        |> recordBreakpoint ret
                        |> printResult interpreter
              }
            , retMsg
            )

        Rewind ret ->
            update (HandleIO ret) { model | interpreter = Running }

        UpdateInputRequest name v ->
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

        UpdateScripts result ->
            case result of
                Ok scripts ->
                    ( { model | scripts = scripts }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SaveScript ->
            let
                query =
                    Lantern.Query.withArguments
                        "UPDATE scripts SET name=$name, code=$code, input=$input, updated_at=datetime('now')"
                        [ ( "$name", model.scriptEditor.name |> Lantern.Query.String )
                        , ( "$code", model.scriptEditor.code |> Lantern.Query.String )
                        , ( "$input", model.scriptEditor.input |> Lantern.Query.String )
                        ]
            in
            ( model, Lantern.writerQuery query ScriptSaved |> Lantern.App.call )

        NewScript ->
            ( { model | scriptEditor = unsavedScript, interpreter = Stopped }, Cmd.none )

        ScriptSaved _ ->
            ( model, Cmd.none )

        CreateScript ->
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

        ScriptCreated result ->
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

        EditScript script ->
            ( { model | scriptEditor = script }, Cmd.none )

        UpdateScriptInput input ->
            let
                oldScriptEditor =
                    model.scriptEditor

                newScriptEditor =
                    { oldScriptEditor | input = input }
            in
            ( { model | scriptEditor = newScriptEditor }, Cmd.none )

        -- Beware: the Apps namespace hijacks this message and launches the inspector
        -- would be nice to make this more explicit but it'll take a lot of refactoring
        InspectValue _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
                |> Element.column [ Element.width Element.fill ]

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
                                    LanternUi.Input.text
                                        context.theme
                                        []
                                        { onChange = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , placeholder = Nothing
                                        , label = Element.Input.labelHidden key
                                        , text = s
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


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
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

        currentScriptButtons =
            Element.row
                [ Element.width Element.fill ]
                [ saveButton, runButton, evalButton ]

        newScriptButton =
            Element.el
                [ Element.width Element.fill ]
                (LanternUi.Input.button context.theme [] { label = Element.text "New script", onPress = Just <| Lantern.App.Message NewScript })

        allScripts =
            List.append
                (if model.scriptEditor.id == Nothing then
                    [ model.scriptEditor ]

                 else
                    []
                )
                model.scripts

        scriptsPanel =
            allScripts
                |> List.map
                    (\script ->
                        let
                            isCurrent =
                                script.id == model.scriptEditor.id
                        in
                        Element.row
                            [ Element.width Element.fill
                            , if isCurrent then
                                Element.Background.color context.theme.bgHighlight

                              else
                                LanternUi.noneAttribute
                            ]
                            [ Element.el [ Element.Events.onClick (Lantern.App.Message <| EditScript script) ] (Element.text (scriptName script))
                            , if isCurrent then
                                currentScriptButtons

                              else
                                Element.none
                            ]
                    )
                |> (::)
                    (if model.scriptEditor.id == Nothing then
                        Element.none

                     else
                        newScriptButton
                    )
                |> Element.column [ Element.width (Element.fillPortion 1), Element.alignTop ]

        saveButton =
            if model.scriptEditor.id == Nothing then
                LanternUi.Input.button context.theme [] { label = Element.text "Save", onPress = Just <| Lantern.App.Message CreateScript }

            else
                LanternUi.Input.button context.theme [] { label = Element.text "Save", onPress = Just <| Lantern.App.Message SaveScript }

        scriptEditor =
            Element.column
                [ Element.width Element.fill ]
                [ LanternUi.Input.text context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateName >> Lantern.App.Message
                    , text = model.scriptEditor.name
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove [] (Element.text "Script name")
                    }
                , LanternUi.Input.multiline context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateScriptInput >> Lantern.App.Message
                    , text = model.scriptEditor.input
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove [] (Element.text "Script input (*input*)")
                    , spellcheck = False
                    }
                , Element.paragraph [] [ Element.text "Code" ]
                , LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    ]
                    { onChange = UpdateCode >> Lantern.App.Message
                    , value = model.scriptEditor.code
                    , language = LanternUi.Input.Enclojure
                    }
                ]

        consoleWithActiveUi =
            activeUi model.interpreter |> Maybe.map (\ui -> UiTrace ui :: model.console) |> Maybe.withDefault model.console

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

                                Breakpoint ( ( Located loc io, env ), thunk ) ->
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
                                        [ Element.paragraph [] [ Element.text "Value breakpoint", rewindButton ]
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
            Element.column [ Element.width Element.fill ]
                [ Element.paragraph [] [ Element.text "REPL" ]
                , LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    , Element.htmlAttribute (Html.Events.preventDefaultOn "keydown" (Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent))
                    ]
                    { onChange = UpdateRepl >> Lantern.App.Message
                    , value = model.repl
                    , language = LanternUi.Input.Enclojure
                    }
                , Element.paragraph [] [ Element.text "Console" ]
                , console
                ]
    in
    LanternUi.columnLayout
        context.theme
        []
        [ Element.row
            [ Element.width Element.fill, Element.spacing 20 ]
            [ scriptsPanel
            , Element.column [ Element.width (Element.fillPortion 5) ] [ scriptEditor, repl ]
            ]
        ]


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
liveQueries _ =
    let
        tablesQuery =
            Lantern.LiveQuery.prepare ( Lantern.Query.Query "SELECT id, input, name, code FROM scripts ORDER BY name" Dict.empty, tableDecoder ) UpdateScripts
    in
    [ tablesQuery ]


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
