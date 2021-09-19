module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located)
import Enclojure.Runtime as Runtime
import Enclojure.Types exposing (Cell(..), Env, Exception(..), IO(..), InputCell(..), InputKey, TextFormat(..), Thunk(..), UI, Value(..))
import Html.Attributes
import Keyboard.Key exposing (Key(..))
import Lantern.App
import LanternUi
import LanternUi.FuzzySelect exposing (FuzzySelect)
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
    = FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | SetCode String
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell
    | HandleIO ( Result (Located Exception) ( Located IO, Env ), Maybe Thunk )
    | NoOp


init : Model
init =
    { code = """
(def words
  (-> (ui [:v-stack
           [:text "Words"]
           [:text-input :words]])
      (get :words)
      string/split-lines))

(defn make-card
  [word]
  (let [translation (ui [:h-stack
                         [:v-stack
                          [:text word]]
                         [:v-stack
                          [:text-input :translation]]])]
    (assoc translation :word word)))

(def cards
  (->> words
       (map make-card)))

cards


"""
    , interpreter = Stopped
    }


type alias UiModel =
    { fuzzySelects : Dict String FuzzySelect, enclojureUi : UI }


type Interpreter
    = Stopped
    | Blocked
    | UI UiModel ( Env, Maybe Thunk )
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

                    ShowUI ui ->
                        ( UI { enclojureUi = ui, fuzzySelects = Dict.empty } ( env, thunk )
                        , Cmd.none
                        )

            Err exception ->
                ( Panic exception, Cmd.none )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        FuzzySelectMessage id selectMsg ->
            case model.interpreter of
                UI ({ fuzzySelects } as ui) args ->
                    let
                        updatedFuzzySelects =
                            Dict.update id (Maybe.map (\state -> LanternUi.FuzzySelect.update selectMsg state)) fuzzySelects
                    in
                    ( { model | interpreter = UI { ui | fuzzySelects = updatedFuzzySelects } args }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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

        UI _ _ ->
            "UI"

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
                                        { label = Element.Input.labelHidden ""
                                        , onQueryChange = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , onInternalMessage = FuzzySelectMessage key >> Lantern.App.Message
                                        , onOptionSelect = TextInput opts >> UpdateInputRequest key >> Lantern.App.Message
                                        , options = []
                                        , placeholder = Nothing
                                        , query = s
                                        , state = Dict.get key fuzzySelects |> Maybe.andThen identity
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
                                { onPress = Just (Lantern.App.Message (HandleIO ( Ok ( Located.fakeLoc (Const (Enclojure.uiToValue ui.enclojureUi)), env ), thunk )))
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
            { onChange = SetCode >> Lantern.App.Message
            , value = model.code
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
