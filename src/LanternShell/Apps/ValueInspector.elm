module LanternShell.Apps.ValueInspector exposing (Flags, Message, Model, init, lanternApp)

import Array exposing (Array)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure.Located
import Enclojure.Runtime
import Enclojure.Types exposing (Number(..), Value(..))
import Enclojure.ValueMap
import Html.Events
import Json.Decode
import Lantern.App
import LanternUi
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Flags =
    { value : Value }


type alias Model =
    { value : Value
    , selectedPath : Maybe (Array Value)
    , hoveredPath : Maybe (Array Value)
    }


type Message
    = SelectPath (Array Value)
    | Nop


init : Maybe Flags -> ( Model, Cmd (Lantern.App.Message Message) )
init flags =
    ( { value = flags |> Maybe.map .value |> Maybe.withDefault Nil
      , selectedPath = Nothing
      , hoveredPath = Nothing
      }
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
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


renderValue : LanternUi.Theme.Theme -> Array Value -> Value -> Element (Lantern.App.Message Message)
renderValue theme path value =
    Element.el
        [ Element.htmlAttribute <|
            Html.Events.stopPropagationOn "click"
                (Json.Decode.succeed
                    ( Lantern.App.Message <| SelectPath path, True )
                )
        ]
        (case value of
            Vector v ->
                Element.column
                    []
                    [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] (Element.text "[")
                    , v
                        |> Array.toList
                        |> List.indexedMap (\i e -> renderValue theme (Array.push (Number (Int i)) path) (Enclojure.Located.getValue e))
                        |> Element.column [ indent ]
                    , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] (Element.text "]")
                    ]

            List v ->
                Element.column
                    []
                    [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "("
                    , v
                        |> List.indexedMap (\i e -> renderValue theme (Array.push (Number (Int i)) path) (Enclojure.Located.getValue e))
                        |> Element.column [ indent ]
                    , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text ")"
                    ]

            Map m ->
                Element.column
                    []
                    [ Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "{"
                    , m
                        |> Enclojure.ValueMap.toList
                        |> List.map
                            (\( k, Enclojure.Located.Located _ v ) ->
                                Element.row
                                    [ Element.width Element.fill, Element.alignTop, Element.spacing 10 ]
                                    [ Element.el [ Element.alignTop ] (renderValue theme (Array.push k path) k)
                                    , renderValue theme (Array.push k path) v
                                    ]
                            )
                        |> Element.column [ indent ]
                    , Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <| Element.text "}"
                    ]

            _ ->
                Element.el [ Element.pointer, Element.mouseOver [ Element.Background.color theme.bgHighlight ] ] <|
                    Element.text (Enclojure.Runtime.inspect value)
        )


view : Context -> Model -> Element (Lantern.App.Message Message)
view ctx model =
    let
        path =
            model.hoveredPath
                |> Maybe.map Just
                |> Maybe.withDefault model.selectedPath
                |> Maybe.map (\v -> Enclojure.Runtime.inspect (Vector (Array.map Enclojure.Located.fakeLoc v)))
                |> Maybe.withDefault " "
    in
    LanternUi.columnLayout
        ctx.theme
        [ Element.height Element.fill, Element.width Element.fill ]
        [ renderValue ctx.theme Array.empty model.value
        , Element.row
            [ Element.alignBottom, Element.width Element.fill ]
            [ Element.Input.text []
                { text = path
                , onChange = \_ -> Lantern.App.Message Nop
                , label = Element.Input.labelLeft [] (Element.text "Path")
                , placeholder = Just (Element.Input.placeholder [] (Element.text "Click on a value to see its path"))
                }
            ]
        ]


lanternApp : Lantern.App.App Context Flags Model Message
lanternApp =
    Lantern.App.app
        { name = "Value inspector"
        , init = init
        , liveQueries = Nothing
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
