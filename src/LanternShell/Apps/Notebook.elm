module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located)
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Env, Exception(..), IO(..), InputRequest, InputType(..), Thunk(..), Value(..))
import Keyboard.Key exposing (Key(..))
import Lantern
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { interpreter : Interpreter
    , code : String
    }


type Message
    = SetCode String
    | Run
    | Stop
    | UpdateInputRequest String InputType
    | HandleIO ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
    | NoOp


init : Model
init =
    { code = """
(def words (:words (input {:words :text})))
"""
    , interpreter = Stopped
    }


type Interpreter
    = Stopped
    | Blocked
    | Input InputRequest ( Env, Maybe Thunk )
    | Running
    | Done ( Value, Env )
    | Panic (Located Exception)


trampoline : ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk ) -> Int -> ( Interpreter, Cmd Message )
trampoline ( result, thunk ) maxdepth =
    if maxdepth <= 0 then
        ( Panic (Located.fakeLoc (Exception "Stack level too deep")), Cmd.none )

    else
        case result of
            Ok ( io, env ) ->
                case Located.getValue io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline (continuation (Located.replace io v) env) (maxdepth - 1)

                            Nothing ->
                                ( Done ( v, env ), Cmd.none )

                    Sleep ms ->
                        ( Running
                        , Process.sleep ms
                            |> Task.perform
                                (\_ ->
                                    HandleIO ( Ok ( Located.replace io (Const Nil), env ), thunk )
                                )
                        )

                    InputRequest inputRequest ->
                        ( Input inputRequest ( env, thunk )
                        , Cmd.none
                        )

            Err exception ->
                ( Panic exception, Cmd.none )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        SetCode code ->
            ( { model | code = code }, Cmd.none )

        Run ->
            let
                ( interpreter, retMsg ) =
                    trampoline (Enclojure.eval model.code) 10000
            in
            ( { model | interpreter = interpreter }, Cmd.map Lantern.App.Message retMsg )

        Stop ->
            ( { model | interpreter = Panic (Located.fakeLoc (Exception "Terminated")) }, Cmd.none )

        HandleIO ret ->
            let
                ( interpreter, cmd ) =
                    trampoline ret 10000
            in
            ( { model | interpreter = interpreter }, cmd |> Cmd.map Lantern.App.Message )

        UpdateInputRequest name v ->
            case model.interpreter of
                Input inputRequest args ->
                    let
                        updatedInputRequest =
                            Dict.update name (Maybe.map (always v)) inputRequest
                    in
                    ( { model | interpreter = Input updatedInputRequest args }, Cmd.none )

                _ ->
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


inspectInterpreter : Interpreter -> String
inspectInterpreter interpreter =
    case interpreter of
        Stopped ->
            "Stopped"

        Running ->
            "Running"

        Blocked ->
            "Blocked"

        Input _ _ ->
            "Input"

        Done ( v, env ) ->
            "Done " ++ Runtime.inspect v ++ " " ++ inspectEnv env

        Panic e ->
            "Panic " ++ Debug.toString e


isRunning : Interpreter -> Bool
isRunning interpreter =
    case interpreter of
        Running ->
            True

        Blocked ->
            True

        Panic _ ->
            False

        Input _ _ ->
            True

        Done _ ->
            False

        Stopped ->
            False


renderCell : Context -> Model -> Element (Lantern.App.Message Message)
renderCell { theme } model =
    let
        overlay =
            case model.interpreter of
                Stopped ->
                    Element.none

                Done _ ->
                    Element.none

                Blocked ->
                    Element.text "Blocked"

                Panic _ ->
                    Element.none

                Input inputRequest ( env, thunk ) ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Background.color theme.bgDefault
                        ]
                        ((inputRequest
                            |> Dict.toList
                            |> List.map
                                (\( k, v ) ->
                                    case v of
                                        TextInput s ->
                                            LanternUi.Input.text theme
                                                []
                                                { onChange = TextInput >> UpdateInputRequest k >> Lantern.App.Message
                                                , text = s
                                                , placeholder = Nothing
                                                , label = Element.Input.labelAbove [] (Element.text k)
                                                }
                                )
                         )
                            ++ [ LanternUi.Input.button theme
                                    []
                                    { onPress = Just (Lantern.App.Message (HandleIO ( Ok ( Located.fakeLoc (Const (Enclojure.inputRequestToValue inputRequest)), env ), thunk )))
                                    , label = Element.text "Submit"
                                    }
                               ]
                        )

                Running ->
                    Element.text "Running"

        ( label, action ) =
            if isRunning model.interpreter then
                ( "Stop", Stop )

            else
                ( "Run", Run )
    in
    Element.column
        []
        [ LanternUi.Input.multiline theme
            [ Element.inFront overlay ]
            { onChange = SetCode >> Lantern.App.Message
            , text = model.code
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Code"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.App.Message action)
            , label = Element.text label
            }
        ]


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    LanternUi.columnLayout
        context.theme
        []
        [ renderCell context model
        , Element.text ("Interpreter: " ++ inspectInterpreter model.interpreter)
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Notebook"
        , init = init
        , view = view
        , update = update
        }
