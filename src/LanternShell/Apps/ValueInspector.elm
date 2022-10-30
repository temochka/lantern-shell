module LanternShell.Apps.ValueInspector exposing (Flags, Message, Model, init, lanternApp)

import Array exposing (Array)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure.Located as Located
import Enclojure.Value as Value exposing (Value)
import Enclojure.ValueMap as ValueMap
import Html.Events
import Json.Decode
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Flags io =
    { value : Value io }


type alias Model io =
    { value : Value io
    , selectedPath : Maybe (Array (Value io))
    , hoveredPath : Maybe (Array (Value io))
    }


type Message io
    = SelectPath (Array (Value io))
    | Nop


init : Maybe (Flags io) -> ( Model io, Cmd (Lantern.App.Message (Message io)) )
init flags =
    ( { value = flags |> Maybe.map .value |> Maybe.withDefault Value.nil
      , selectedPath = Nothing
      , hoveredPath = Nothing
      }
    , Cmd.none
    )


update : Message io -> Model io -> ( Model io, Cmd (Lantern.App.Message (Message io)) )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        SelectPath path ->
            ( { model | selectedPath = Just path }, Cmd.none )


paddingLeft : Int -> Element.Attribute (Lantern.App.Message msg)
paddingLeft val =
    Element.paddingEach { top = 0, right = 0, bottom = 0, left = val }


indent : Element.Attribute (Lantern.App.Message msg)
indent =
    paddingLeft 20


renderValue : LanternUi.Theme.Theme -> Array (Value io) -> Value io -> Element (Lantern.App.Message (Message io))
renderValue theme path value =
    Element.el
        [ Element.htmlAttribute <|
            Html.Events.stopPropagationOn "click"
                (Json.Decode.succeed
                    ( Lantern.App.Message <| SelectPath path, True )
                )
        ]
        (value
            |> Value.tryOneOf
                [ Value.tryVector
                    >> Maybe.map
                        (\v ->
                            Element.column
                                []
                                [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] (Element.text "[")
                                , v
                                    |> Array.toList
                                    |> List.indexedMap (\i e -> renderValue theme (Array.push (Value.int i) path) (Located.getValue e))
                                    |> Element.column [ indent ]
                                , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] (Element.text "]")
                                ]
                        )
                , Value.tryList
                    >> Maybe.map
                        (\v ->
                            Element.column
                                []
                                [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "("
                                , v
                                    |> List.indexedMap (\i e -> renderValue theme (Array.push (Value.int i) path) (Located.getValue e))
                                    |> Element.column [ indent ]
                                , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text ")"
                                ]
                        )
                , Value.tryMap
                    >> Maybe.map
                        (\m ->
                            Element.column
                                []
                                [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "{"
                                , m
                                    |> ValueMap.toList
                                    |> List.map
                                        (\( k, Located.Located _ v ) ->
                                            Element.row
                                                [ Element.width Element.fill, Element.alignTop, Element.spacing 10 ]
                                                [ Element.el [ Element.alignTop ] (renderValue theme (Array.push k path) k)
                                                , renderValue theme (Array.push k path) v
                                                ]
                                        )
                                    |> Element.column [ indent ]
                                , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "}"
                                ]
                        )
                ]
            |> (\me ->
                    case me of
                        Just e ->
                            e

                        Nothing ->
                            Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <|
                                Element.text (Value.inspect value)
               )
        )


view : Context -> Model io -> Element (Lantern.App.Message (Message io))
view ctx model =
    let
        path =
            model.hoveredPath
                |> Maybe.map Just
                |> Maybe.withDefault model.selectedPath
                |> Maybe.map (\v -> Value.inspect (Value.vector v))
                |> Maybe.withDefault " "
    in
    LanternUi.columnLayout
        ctx.theme
        [ Element.height Element.fill, Element.width Element.fill, Element.scrollbarX ]
        [ renderValue ctx.theme Array.empty model.value
        , Element.row
            [ Element.alignBottom, Element.width Element.fill ]
            [ LanternUi.Input.text ctx.theme
                [ Element.width Element.fill ]
                { text = path
                , onChange = \_ -> Lantern.App.Message Nop
                , label = Element.Input.labelLeft [] (Element.text "Path")
                , placeholder = Just (Element.Input.placeholder [] (Element.text "Click on a value to see its path"))
                }
            ]
        ]


lanternApp : Lantern.App.App Context (Flags io) (Model io) (Message io)
lanternApp =
    Lantern.App.app
        { name = "Value inspector"
        , init = init
        , liveQueries = Nothing
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        , initWindow = \_ _ model -> ( model, Cmd.none )
        }
