module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

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


type DocumentNode
    = TextNode (DocumentNodeContainer { content : String })
    | ScriptNode (DocumentNodeContainer { script : String, result : ScriptResult })
    | ListNode (DocumentNodeContainer { valueNodes : List NodeId })
    | ValueNode (DocumentNodeContainer { value : String })
    | TypeNode (DocumentNodeContainer { fieldNode : NodeId })
    | FieldNode (DocumentNodeContainer { name : String, valueNode : NodeId })
    | MapNode (DocumentNodeContainer { fieldNodes : List NodeId })


type alias NodeEditor =
    { editedNode : DocumentNode, value : String }


type alias Model =
    { interpreter : Interpreter
    , rootNodes : List NodeId
    , nodes : Dict NodeId DocumentNode
    , code : String
    , nodeEditor : Maybe NodeEditor
    , nextNodeId : NodeId
    }


type Message
    = FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | SetCode String
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell
    | EditNode { nodeId : NodeId, parentId : NodeId }
    | DeleteNode NodeId
    | AddFieldNode { parentId : NodeId, afterNodeId : NodeId }
    | UpdateEditorValue String
    | SaveEditor
    | HandleIO NodeId ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk )
    | NoOp


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( { code = """
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
      , nodes =
            Dict.fromList
                [ ( 0, TypeNode { id = 0, parentId = -1, fieldNode = 1 } )
                , ( 1, FieldNode { id = 1, parentId = 0, name = "Note", valueNode = 2 } )
                , ( 2, ValueNode { id = 2, parentId = 1, value = "" } )
                ]
      , rootNodes = [ 0 ]
      , nodeEditor = Nothing
      , nextNodeId = 3
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


trampoline : NodeId -> ( Result ( Located Exception, Env ) ( Located IO, Env ), Maybe Thunk ) -> Int -> ( ScriptResult, Cmd Message )
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

                    Savepoint _ ->
                        ( ScriptError "Not implemented"
                        , Cmd.none
                        )

                    ShowUI _ ->
                        ( ScriptError "Not implemented"
                        , Cmd.none
                        )

                    Http _ ->
                        ( ScriptError "Not implemented"
                        , Cmd.none
                        )

                    ReadField _ ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline nodeId (continuation (Located.replace io (String "apple")) env) (maxdepth - 1)

                            Nothing ->
                                ( ScriptResult "\"apple\"", Cmd.none )

            Err ( Located _ (Exception err), _ ) ->
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

        SetCode code ->
            ( { model | code = code }, Cmd.none )

        Run ->
            ( model, Cmd.none )

        Stop ->
            ( { model | interpreter = Panic (Located.fakeLoc (Exception "Terminated")) }, Cmd.none )

        HandleIO nodeId ret ->
            let
                ( scriptResult, cmd ) =
                    trampoline nodeId ret 10000

                updatedNodes =
                    Dict.update
                        nodeId
                        (Maybe.map
                            (\node ->
                                case node of
                                    ScriptNode attrs ->
                                        ScriptNode { attrs | result = scriptResult }

                                    _ ->
                                        node
                            )
                        )
                        model.nodes
            in
            ( { model | nodes = updatedNodes }, cmd |> Cmd.map Lantern.App.Message )

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

        EditNode { nodeId, parentId } ->
            let
                nodeEditor =
                    let
                        node =
                            Dict.get nodeId model.nodes
                                |> Maybe.withDefault (ValueNode { id = nodeId, parentId = parentId, value = "" })
                    in
                    case node of
                        MapNode _ ->
                            Nothing

                        TypeNode _ ->
                            Nothing

                        FieldNode { name } ->
                            Just { editedNode = node, value = name }

                        TextNode { content } ->
                            Just { editedNode = node, value = content }

                        ValueNode { value } ->
                            Just { editedNode = node, value = value }

                        ScriptNode { script } ->
                            Just { editedNode = node, value = script }

                        ListNode _ ->
                            Nothing
            in
            ( { model | nodeEditor = nodeEditor }, Cmd.none )

        UpdateEditorValue val ->
            let
                updatedModel =
                    model.nodeEditor
                        |> Maybe.map
                            (\nodeEditor ->
                                case ( nodeEditor.editedNode, val ) of
                                    ( ValueNode { id, parentId }, "\"" ) ->
                                        let
                                            updatedNodeEditor =
                                                { nodeEditor
                                                    | editedNode = TextNode { id = id, parentId = parentId, content = "" }
                                                    , value = ""
                                                }
                                        in
                                        { model | nodeEditor = Just updatedNodeEditor }

                                    ( ValueNode { id, parentId, value }, "*" ) ->
                                        let
                                            listNode =
                                                ListNode { id = id, parentId = parentId, valueNodes = [ model.nextNodeId ] }

                                            valueNode =
                                                ValueNode { id = model.nextNodeId, parentId = id, value = value }

                                            updatedNodeEditor =
                                                { nodeEditor
                                                    | editedNode = valueNode
                                                    , value = ""
                                                }
                                        in
                                        { model
                                            | nodeEditor = Just updatedNodeEditor
                                            , nextNodeId = model.nextNodeId + 1
                                            , nodes =
                                                model.nodes
                                                    |> Dict.insert id listNode
                                                    |> Dict.insert model.nextNodeId valueNode
                                        }

                                    ( ValueNode { id, parentId }, "{" ) ->
                                        let
                                            fieldNodeId =
                                                model.nextNodeId

                                            valueNodeId =
                                                model.nextNodeId + 1

                                            mapNode =
                                                MapNode { id = id, parentId = parentId, fieldNodes = [ fieldNodeId ] }

                                            fieldNode =
                                                FieldNode { id = fieldNodeId, parentId = id, name = "", valueNode = valueNodeId }

                                            valueNode =
                                                ValueNode { id = valueNodeId, parentId = fieldNodeId, value = "" }

                                            updatedNodeEditor =
                                                { nodeEditor
                                                    | editedNode = fieldNode
                                                    , value = ""
                                                }
                                        in
                                        { model
                                            | nodeEditor = Just updatedNodeEditor
                                            , nextNodeId = model.nextNodeId + 2
                                            , nodes =
                                                model.nodes
                                                    |> Dict.insert id mapNode
                                                    |> Dict.insert fieldNodeId fieldNode
                                                    |> Dict.insert valueNodeId valueNode
                                        }

                                    ( ValueNode { id, parentId }, "::" ) ->
                                        let
                                            fieldNodeId =
                                                model.nextNodeId

                                            valueNodeId =
                                                model.nextNodeId + 1

                                            typeNode =
                                                TypeNode { id = id, parentId = parentId, fieldNode = model.nextNodeId }

                                            fieldNode =
                                                FieldNode { id = fieldNodeId, parentId = id, name = "", valueNode = valueNodeId }

                                            valueNode =
                                                ValueNode { id = valueNodeId, parentId = fieldNodeId, value = "" }

                                            updatedNodeEditor =
                                                { nodeEditor
                                                    | editedNode = fieldNode
                                                    , value = ""
                                                }
                                        in
                                        { model
                                            | nodeEditor = Just updatedNodeEditor
                                            , nextNodeId = model.nextNodeId + 2
                                            , nodes =
                                                model.nodes
                                                    |> Dict.insert id typeNode
                                                    |> Dict.insert fieldNodeId fieldNode
                                                    |> Dict.insert valueNodeId valueNode
                                        }

                                    ( ValueNode { id, parentId }, "=" ) ->
                                        let
                                            updatedNodeEditor =
                                                { nodeEditor
                                                    | editedNode = ScriptNode { id = id, parentId = parentId, script = "", result = ScriptRefreshPending }
                                                    , value = ""
                                                }
                                        in
                                        { model | nodeEditor = Just updatedNodeEditor }

                                    _ ->
                                        let
                                            updatedNodeEditor =
                                                { nodeEditor | value = val }
                                        in
                                        { model | nodeEditor = Just updatedNodeEditor }
                            )
                        |> Maybe.withDefault model
            in
            ( updatedModel, Cmd.none )

        DeleteNode id ->
            model.nodeEditor
                |> Maybe.map
                    (\{ editedNode } ->
                        case editedNode of
                            TextNode attrs ->
                                let
                                    newValueNode =
                                        ValueNode { id = attrs.id, parentId = attrs.parentId, value = "" }

                                    updatedNodeEditor =
                                        { editedNode = newValueNode, value = "" }
                                in
                                ( { model | nodeEditor = Just updatedNodeEditor, nodes = Dict.insert id newValueNode model.nodes }, Cmd.none )

                            _ ->
                                ( { model | nodeEditor = Nothing, nodes = Dict.remove id model.nodes }, Cmd.none )
                    )
                |> Maybe.withDefault ( { model | nodes = Dict.remove id model.nodes }, Cmd.none )

        AddFieldNode { parentId, afterNodeId } ->
            let
                ( modelPostSave, _ ) =
                    update SaveEditor model

                fieldNodeId =
                    modelPostSave.nextNodeId

                valueNodeId =
                    modelPostSave.nextNodeId + 1

                fieldNode =
                    FieldNode { id = fieldNodeId, parentId = parentId, name = "", valueNode = valueNodeId }

                valueNode =
                    ValueNode { id = valueNodeId, parentId = fieldNodeId, value = "" }

                nodes =
                    modelPostSave.nodes
                        |> Dict.update
                            parentId
                            (\mNode ->
                                mNode
                                    |> Maybe.map
                                        (\n ->
                                            case n of
                                                MapNode ({ fieldNodes } as attrs) ->
                                                    let
                                                        ( fieldsBefore, parent, fieldsAfter ) =
                                                            List.Extra.splitWhen ((==) afterNodeId) fieldNodes
                                                                |> Maybe.map
                                                                    (\( before, after ) ->
                                                                        case after of
                                                                            head :: rest ->
                                                                                ( before, [ head ], rest )

                                                                            _ ->
                                                                                ( before, [], after )
                                                                    )
                                                                |> Maybe.withDefault ( fieldNodes, [], [] )

                                                        newFields =
                                                            List.concat [ fieldsBefore, parent, [ fieldNodeId ], fieldsAfter ]
                                                    in
                                                    MapNode { attrs | fieldNodes = newFields }

                                                _ ->
                                                    n
                                        )
                            )
                        |> Dict.insert fieldNodeId fieldNode
                        |> Dict.insert valueNodeId valueNode
            in
            ( { modelPostSave
                | nodes = nodes
                , nextNodeId = modelPostSave.nextNodeId + 2
                , nodeEditor = Just { editedNode = fieldNode, value = "" }
              }
            , Cmd.none
            )

        SaveEditor ->
            case model.nodeEditor of
                Just { editedNode, value } ->
                    let
                        ( updatedNode, nodeMsg ) =
                            case editedNode of
                                MapNode _ ->
                                    ( editedNode, Cmd.none )

                                TypeNode _ ->
                                    ( editedNode, Cmd.none )

                                FieldNode n ->
                                    ( FieldNode { n | name = value }, Cmd.none )

                                TextNode n ->
                                    ( TextNode { n | content = value }, Cmd.none )

                                ListNode _ ->
                                    ( editedNode, Cmd.none )

                                ValueNode n ->
                                    ( ValueNode { n | value = value }, Cmd.none )

                                ScriptNode n ->
                                    let
                                        ( scriptResult, m ) =
                                            trampoline n.id (Enclojure.eval Runtime.emptyEnv value) 10000
                                    in
                                    ( ScriptNode { n | script = value, result = scriptResult }
                                    , m |> Cmd.map Lantern.App.Message
                                    )

                        nodes =
                            Dict.insert (extractId editedNode)
                                updatedNode
                                model.nodes
                    in
                    ( { model | nodes = nodes, nodeEditor = Nothing }, nodeMsg )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


extractId : DocumentNode -> NodeId
extractId node =
    case node of
        TypeNode { id } ->
            id

        TextNode { id } ->
            id

        ValueNode { id } ->
            id

        ListNode { id } ->
            id

        ScriptNode { id } ->
            id

        MapNode { id } ->
            id

        FieldNode { id } ->
            id


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

                            _ ->
                                Element.none
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
            { onChange = SetCode >> Lantern.App.Message
            , value = model.code
            , language = LanternUi.Input.Enclojure
            , label = Nothing
            }
        , LanternUi.Input.button context.theme
            []
            { onPress = Just (Lantern.App.Message action)
            , label = Element.text label
            }
        ]


renderEditor : Context -> NodeEditor -> Element (Lantern.App.Message Message)
renderEditor ctx { editedNode, value } =
    let
        handleKeyPress event =
            case ( event.keyCode, event.metaKey, event.altKey ) of
                ( Keyboard.Key.Backspace, False, False ) ->
                    if value == "" then
                        ( DeleteNode (extractId editedNode) |> Lantern.App.Message, False )

                    else
                        ( NoOp |> Lantern.App.Message, False )

                ( Keyboard.Key.Enter, True, False ) ->
                    ( SaveEditor |> Lantern.App.Message, True )

                ( Keyboard.Key.Enter, False, True ) ->
                    case editedNode of
                        FieldNode { id, parentId } ->
                            ( AddFieldNode { parentId = parentId, afterNodeId = id } |> Lantern.App.Message, True )

                        _ ->
                            ( NoOp |> Lantern.App.Message, False )

                _ ->
                    ( NoOp |> Lantern.App.Message, False )

        isScript =
            case editedNode of
                ScriptNode _ ->
                    True

                _ ->
                    False

        attributes =
            [ Element.width (500 |> Element.px)
            , Element.htmlAttribute <| Html.Attributes.autofocus True
            , Element.htmlAttribute (Html.Events.preventDefaultOn "keydown" (Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent))
            ]
    in
    if isScript then
        LanternUi.Input.code
            ctx.theme
            attributes
            { onChange = UpdateEditorValue >> Lantern.App.Message
            , language = LanternUi.Input.Enclojure
            , value = value
            , label = Nothing
            }

    else
        Element.Input.multiline
            attributes
            { onChange = UpdateEditorValue >> Lantern.App.Message
            , placeholder = Just (Element.Input.placeholder [] (Element.text "nil"))
            , text = value
            , label = Element.Input.labelHidden "editor"
            , spellcheck = False
            }


renderDocumentNode : Context -> Model -> NodeId -> NodeId -> Element (Lantern.App.Message Message)
renderDocumentNode ctx model parentId nodeId =
    Dict.get nodeId model.nodes
        |> Maybe.withDefault (ValueNode { id = nodeId, parentId = parentId, value = "" })
        |> (\node ->
                let
                    editOnDoubleClick =
                        Element.Events.onDoubleClick (Lantern.App.Message (EditNode { nodeId = nodeId, parentId = parentId }))

                    editor =
                        model.nodeEditor
                            |> Maybe.andThen
                                (\e ->
                                    if nodeId == extractId e.editedNode then
                                        Just e

                                    else
                                        Nothing
                                )

                    actualNode =
                        editor
                            |> Maybe.map .editedNode
                            |> Maybe.withDefault node
                in
                case actualNode of
                    MapNode n ->
                        List.map (renderDocumentNode ctx model nodeId) n.fieldNodes |> Element.column []

                    TypeNode n ->
                        renderDocumentNode ctx model nodeId n.fieldNode

                    FieldNode { id, name, valueNode } ->
                        Element.column
                            []
                            [ Element.paragraph []
                                [ Element.text "\""
                                , editor
                                    |> Maybe.map (renderEditor ctx)
                                    |> Maybe.withDefault (Element.el [ editOnDoubleClick ] (Element.text name))
                                , Element.el [] (Element.text "\":")
                                ]
                            , Element.el [ Element.paddingEach { left = 10, right = 0, top = 0, bottom = 0 } ] (renderDocumentNode ctx model id valueNode)
                            ]

                    ListNode { valueNodes } ->
                        valueNodes
                            |> List.map (renderDocumentNode ctx model nodeId)
                            |> List.map (\n -> Element.paragraph [] [ Element.text "• ", n ])
                            |> Element.column []

                    ValueNode { value } ->
                        editor
                            |> Maybe.map (renderEditor ctx)
                            |> Maybe.withDefault
                                (Element.el
                                    [ editOnDoubleClick ]
                                    (if String.isEmpty value then
                                        Element.text "nil"

                                     else
                                        Element.text value
                                    )
                                )

                    TextNode { content } ->
                        Element.paragraph
                            []
                            [ Element.text "\""
                            , editor
                                |> Maybe.map (renderEditor ctx)
                                |> Maybe.withDefault (Element.el [ editOnDoubleClick ] (Element.text content))
                            , Element.text "\""
                            ]

                    ScriptNode { result } ->
                        let
                            content =
                                case result of
                                    ScriptError e ->
                                        Element.text ("Error: " ++ e)

                                    ScriptRefreshPending ->
                                        Element.text "Pending refresh..."

                                    ScriptRunning ->
                                        Element.text "Running..."

                                    ScriptResult r ->
                                        Element.text r
                        in
                        Element.paragraph
                            []
                            [ Element.text "= "
                            , editor
                                |> Maybe.map (renderEditor ctx)
                                |> Maybe.withDefault (Element.el [ editOnDoubleClick ] content)
                            ]
           )


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    model.rootNodes
        |> List.map (renderDocumentNode context model -1)
        |> LanternUi.columnLayout context.theme []


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.app
        { name = "Notebook"
        , init = \_ -> init
        , view = view
        , update = update
        , liveQueries = Nothing
        , subscriptions = always Sub.none
        }
