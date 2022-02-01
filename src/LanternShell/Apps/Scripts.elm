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
import Html.Attributes
import Html.Events
import Json.Decode
import Keyboard.Event
import Keyboard.Key exposing (Key(..))
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternUi
import LanternUi.FuzzySelect exposing (FuzzySelect)
import LanternUi.Input
import LanternUi.Theme
import List.Extra
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias NodeId =
    Int


type alias DocumentNodeContainer n =
    { n | id : NodeId, parentId : NodeId }


type ScriptResult
    = ScriptRefreshPending
    | ScriptRunning
    | ScriptError String
    | ScriptResult String


type alias Model =
    { interpreter : Interpreter
    , scripts : List Script
    , scriptEditor : Script
    }


type Message
    = FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell
    | UpdateScripts (Result Lantern.Error (List Script))
    | HandleIO NodeId ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
    | CreateScript
    | ScriptCreated (Result Lantern.Error Lantern.Query.WriterResult)
    | SaveScript
    | ScriptSaved (Result Lantern.Error Lantern.Query.WriterResult)
    | EditScript Script
    | UpdateName String
    | UpdateCode String
    | NoOp


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( { interpreter = Stopped
      , scripts = []
      , scriptEditor = { id = Nothing, code = "", name = "", input = "" }
      }
    , Cmd.none
    )


type alias UiModel =
    { fuzzySelects : Dict String FuzzySelect, enclojureUi : UI }


type Interpreter
    = Stopped
    | Blocked
    | UI UiModel ( Env, Maybe Thunk )
    | Running
    | Done ( Value, Env )
    | Panic (Located Exception)


trampoline : NodeId -> ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk ) -> Int -> ( ScriptResult, Cmd Message )
trampoline nodeId ( result, thunk ) maxdepth =
    if maxdepth <= 0 then
        ( ScriptError "Stack level too deep", Cmd.none )

    else
        case result of
            Ok ( io, env ) ->
                case Located.getValue io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline nodeId (continuation (Located.replace io v) env) (maxdepth - 1)

                            Nothing ->
                                ( ScriptResult (Runtime.inspect v), Cmd.none )

                    Sleep ms ->
                        ( ScriptRunning
                        , Process.sleep ms
                            |> Task.perform
                                (\_ ->
                                    HandleIO nodeId ( Ok ( Located.replace io (Const Nil), env ), thunk )
                                )
                        )

                    ShowUI _ ->
                        ( ScriptError "Not implemented"
                        , Cmd.none
                        )

                    ReadField _ ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline nodeId (continuation (Located.replace io (String "apple")) env) (maxdepth - 1)

                            Nothing ->
                                ( ScriptResult "\"apple\"", Cmd.none )

            Err (Located _ (Exception err)) ->
                ( ScriptError err, Cmd.none )


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

        UpdateName name ->
            let
                scriptEditor =
                    model.scriptEditor

                editor =
                    { scriptEditor | name = name }
            in
            ( { model | scriptEditor = editor }, Cmd.none )

        Run ->
            ( model, Cmd.none )

        Stop ->
            ( { model | interpreter = Panic (Located.fakeLoc (Exception "Terminated")) }, Cmd.none )

        HandleIO scriptId ret ->
            let
                ( scriptResult, cmd ) =
                    trampoline scriptId ret 10000
            in
            ( model, Cmd.none )

        UpdateInputRequest name v ->
            case model.interpreter of
                UI ({ enclojureUi } as ui) args ->
                    let
                        updatedInputs =
                            Dict.update name (Maybe.map (always v)) ui.enclojureUi.inputs

                        updatedUi =
                            { enclojureUi | inputs = updatedInputs }
                    in
                    ( { model | interpreter = UI { ui | enclojureUi = updatedUi } args }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateScripts result ->
            case result of
                Ok scripts ->
                    ( { model | scripts = scripts }, Cmd.none )

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
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
                                        , label = Element.Input.labelHidden ""
                                        , text = s
                                        }

                                else
                                    LanternUi.FuzzySelect.fuzzySelect
                                        context.theme
                                        { label = Element.Input.labelLeft [] (Element.text "foo")
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


renderCell : Context -> Model -> Element (Lantern.App.Message Message)
renderCell context model =
    let
        overlayLayout el =
            Element.el
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.Background.color context.theme.bgDefault
                , Element.htmlAttribute (Html.Attributes.style "z-index" "10")
                , Element.padding 10
                ]
                el

        overlay =
            case model.interpreter of
                Stopped ->
                    Element.none

                Done _ ->
                    Element.none

                Blocked ->
                    overlayLayout <| Element.text "Blocked"

                Panic _ ->
                    Element.none

                UI ui ( env, thunk ) ->
                    overlayLayout <|
                        Element.column
                            [ Element.width Element.fill ]
                            [ renderUI context ui
                            , LanternUi.Input.button context.theme
                                []
                                { onPress = Nothing
                                , label = Element.text "Submit"
                                }
                            ]

                Running ->
                    overlayLayout <| Element.text "Running"

        ( label, action ) =
            if isRunning model.interpreter then
                ( "Stop", Stop )

            else
                ( "Run", Run )
    in
    Element.column
        [ Element.width Element.fill ]
        [ LanternUi.Input.code context.theme
            [ Element.inFront overlay, Element.width Element.fill, Element.height (Element.px 600) ]
            { onChange = Debug.todo
            , value = ""
            , language = LanternUi.Input.Enclojure
            }
        , LanternUi.Input.button context.theme
            []
            { onPress = Just (Lantern.App.Message action)
            , label = Element.text label
            }
        ]


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    let
        scriptsPanel =
            model.scripts
                |> List.map
                    (\script ->
                        Element.el
                            [ Element.width Element.fill
                            , Element.Events.onClick (Lantern.App.Message <| EditScript script)
                            , if script.id == model.scriptEditor.id then
                                Element.Background.color context.theme.bgActive

                              else
                                LanternUi.noneAttribute
                            ]
                            (Element.text script.name)
                    )
                |> Element.column [ Element.width (Element.fillPortion 1) ]

        saveButton =
            if model.scriptEditor.id == Nothing then
                LanternUi.Input.button context.theme [] { label = Element.text "Create", onPress = Just <| Lantern.App.Message CreateScript }

            else
                LanternUi.Input.button context.theme [] { label = Element.text "Save", onPress = Just <| Lantern.App.Message SaveScript }

        scriptEditor =
            Element.column
                [ Element.width (Element.fillPortion 5) ]
                [ LanternUi.Input.text context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateName >> Lantern.App.Message
                    , text = model.scriptEditor.name
                    , placeholder = Just (Element.Input.placeholder [] (Element.text "Script name"))
                    , label = Element.Input.labelHidden "Script name"
                    }
                , LanternUi.Input.code context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateCode >> Lantern.App.Message
                    , value = model.scriptEditor.code
                    , language = LanternUi.Input.Enclojure
                    }
                , saveButton
                ]
    in
    Element.row
        [ Element.width Element.fill ]
        [ scriptsPanel
        , scriptEditor
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


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.app
        { name = "Scripts"
        , init = init
        , view = view
        , update = update
        , liveQueries = Just liveQueries
        , subscriptions = always Sub.none
        }
