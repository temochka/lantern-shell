module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located)
import Enclojure.Runtime as Runtime exposing (IO(..))
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
    | HandleIO ( Result (Located Runtime.Exception) (Located Runtime.IO), Maybe Enclojure.Thunk )


init : Model
init =
    { code = """[
    (def foo 42)
    (+ 1 2)
    (- 10 9 8)
    (sleep 10)
    (if true (* 2 2) (/ 2 2))
    nil
]
    """
    , interpreter = Stopped
    }


type Interpreter
    = Stopped
    | Running
    | Done Runtime.Value
    | Panic (Located Runtime.Exception)
    | StackOverflow


trampoline : ( Result (Located Runtime.Exception) (Located Runtime.IO), Maybe Enclojure.Thunk ) -> Int -> ( Interpreter, Cmd Message )
trampoline ( result, thunk ) maxdepth =
    if maxdepth <= 0 then
        ( StackOverflow, Cmd.none )

    else
        case result of
            Ok io ->
                case Located.getValue io of
                    Const v ->
                        case thunk of
                            Just (Enclojure.Thunk continuation) ->
                                trampoline (continuation (Located.replace io v)) (maxdepth - 1)

                            Nothing ->
                                ( Done v, Cmd.none )

                    Sleep ms ->
                        ( Running
                        , Process.sleep ms
                            |> Task.perform
                                (\_ ->
                                    HandleIO ( Ok <| Located.replace io (Const Runtime.Nil), thunk )
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
                    trampoline (Enclojure.eval model.code) 100
            in
            ( { model | interpreter = interpreter }, cmd |> Cmd.map Lantern.App.Message )

        HandleIO ret ->
            let
                ( interpreter, cmd ) =
                    trampoline ret 100
            in
            ( { model | interpreter = interpreter }, cmd |> Cmd.map Lantern.App.Message )


inspectInterpreter : Interpreter -> String
inspectInterpreter interpreter =
    case interpreter of
        Stopped ->
            "Stopped"

        Running ->
            "Running"

        Done v ->
            "Done " ++ Runtime.inspect v

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
