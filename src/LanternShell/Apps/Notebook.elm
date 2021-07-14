module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Dict
import Element exposing (Element)
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located)
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Env, Exception(..), IO(..), Thunk(..), Value(..))
import Keyboard.Key exposing (Key(..))
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { code : String
    , interpreter : Interpreter
    }


type Message
    = SetCode String
    | Eval
    | HandleIO ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )


init : Model
init =
    { code = """[
    (def foo 42)
    foo
    (+ 1 2)
    (- 10 9 8)
    (sleep 10)
    (if true (* 2 2) (/ 2 2))
    nil
    (quote (a b c d))
    (let [foo 43 bar (+ 1 1)]
        (def userfn (fn userfn [] [foo bar 666]))
        [foo bar])
    (def three-arg-fn (fn [a b c] [a b c]))
    (three-arg-fn 1 2 3)
    (def var-arg-fn (fn var-arg-fn [a b & rest] [a b rest]))
    (var-arg-fn 1 2 3 4 5 6)
    (def recfn (fn [continue] (if continue (recur false) (quote done))))
    (recfn true)
    (userfn)
    foo
    (def multi-fn (fn ([] (multi-fn 666)) ([a & rest] [a rest])))
    (multi-fn)
    (multi-fn 1 2 3 4 5 6)
    (and 1 true (quote foo) [] false)
    {:foo "bar" :buz "yay"}
    #{1 2 :foo 42.0}
]
    """
    , interpreter = Stopped
    }


type Interpreter
    = Stopped
    | Running
    | Done Value Env
    | Panic (Located Exception)
    | StackOverflow


trampoline : ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk ) -> Int -> ( Interpreter, Cmd Message )
trampoline ( result, thunk ) maxdepth =
    if maxdepth <= 0 then
        ( StackOverflow, Cmd.none )

    else
        case result of
            Ok ( io, env ) ->
                case Located.getValue io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline (continuation (Located.replace io v) env) (maxdepth - 1)

                            Nothing ->
                                ( Done v env, Cmd.none )

                    Sleep ms ->
                        ( Running
                        , Process.sleep ms
                            |> Task.perform
                                (\_ ->
                                    HandleIO ( Ok ( Located.replace io (Const Nil), env ), thunk )
                                )
                        )

            Err exception ->
                ( Panic exception, Cmd.none )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        SetCode code ->
            ( { model | code = code }, Cmd.none )

        Eval ->
            let
                ( interpreter, cmd ) =
                    trampoline (Enclojure.eval model.code) 1000
            in
            ( { model | interpreter = interpreter }, cmd |> Cmd.map Lantern.App.Message )

        HandleIO ret ->
            let
                ( interpreter, cmd ) =
                    trampoline ret 1000
            in
            ( { model | interpreter = interpreter }, cmd |> Cmd.map Lantern.App.Message )


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

        Done v env ->
            "Done " ++ Runtime.inspect v ++ " " ++ inspectEnv env

        StackOverflow ->
            "Stack overflow"

        Panic e ->
            "Panic " ++ Debug.toString e


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } model =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = SetCode >> Lantern.App.Message
            , text = model.code
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Code"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.App.Message Eval)
            , label = Element.text "Eval"
            }
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
