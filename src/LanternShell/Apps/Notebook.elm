module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located)
import Enclojure.Runtime as Runtime exposing (IO(..))
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
    { code : String
    , interpreter : Interpreter
    }


type Message
    = SetCode String
    | Eval
    | HandleIO ( Located Runtime.IO, Maybe Enclojure.Thunk )



-- | ResumeIO (Enclojure.Runtime.Env -> ( Enclojure.Runtime.Env, Result Enclojure.Runtime.Exception Enclojure.IO ))


init : Model
init =
    { code = ""
    , interpreter = Stopped
    }


convergeResult : Result a a -> a
convergeResult result =
    case result of
        Ok v ->
            v

        Err v ->
            v


type Interpreter
    = Stopped
    | Running
    | Done Runtime.Value
    | StackOverflow


trampoline : ( Located Runtime.IO, Maybe Enclojure.Thunk ) -> Int -> ( Interpreter, Cmd Message )
trampoline ( io, thunk ) maxdepth =
    if maxdepth <= 0 then
        ( StackOverflow, Cmd.none )

    else
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
                            HandleIO ( Located.replace io (Const Runtime.Nil), thunk )
                        )
                )



-- stepInterpreter : Interpreter -> Result (Located Runtime.Exception) ( Enclojure.IO, Maybe Enclojure.Continuation ) -> ( Interpreter, Cmd Message )
-- stepInterpreter interpreter result =
--     case interpreter of
--         Running model ->
--             result
--                 |> Result.map
--                     (\( io, continuation ) ->
--                         continuation
--                             |> Maybe.map
--                                 (\(Enclojure.Continuation c) ->
--                                     case io of
--                                         IO.Const value ->
--                                             c value |> stepInterpreter interpreter
--                                 )
--                             |> Maybe.withDefault ( Done model (Ok Runtime.Nil), Cmd.none )
--                     )
--                 |> Result.mapError
--                     (\e -> ())
--         Stopped ->
--             ( interpreter, Cmd.none )
--         Done _ _ ->
--             ( interpreter, Cmd.none )


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



-- ResumeIO continuation ->
--     case model.interpreter of
--         Running interpreter ->
--             let
--                 ( env, result ) =
--                     continuation interpreter.env
--                 updatedInterpreter =
--                     { interpreter | env = env }
--             in
--             case result of
--                 Ok io ->
--                     let
--                         ( newInterpreter, cmd ) =
--                             stepInterpreter io (Running updatedInterpreter)
--                     in
--                     ( { model | interpreter = newInterpreter }, cmd )
--                 Err exception ->
--                     ( { model | interpreter = Done updatedInterpreter (Err exception) }, Cmd.none )
-- _ ->
--     ( model, Cmd.none )


inspectInterpreter : Interpreter -> String
inspectInterpreter interpreter =
    case interpreter of
        Stopped ->
            "Stopped"

        Running ->
            "Running"

        Done v ->
            "Done " ++ Debug.toString v

        StackOverflow ->
            "Stack overflow"


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
