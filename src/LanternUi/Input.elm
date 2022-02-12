module LanternUi.Input exposing (Language(..), button, code, multiline, password, text)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import LanternUi.Theme exposing (Theme)


inputWidth : Element.Attribute msg
inputWidth =
    Element.fill |> Element.maximum 600 |> Element.width


inputAttrs : Theme -> List (Element.Attribute msg)
inputAttrs theme =
    [ inputWidth
    , Element.Font.color theme.fontDefault
    , Element.Background.color theme.bgDefault
    , Element.Border.color theme.borderDefault
    , Element.Border.solid
    , Element.Border.width 1
    ]


button :
    Theme
    -> List (Element.Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button theme extraAttrs mainAttrs =
    Element.Input.button
        ([ Element.Background.color theme.bgDefault
         , Element.padding 7
         , Element.Border.rounded 5
         , Element.Border.solid
         , Element.Border.width 1
         , Element.mouseOver
            [ Element.Background.color theme.bgHighlight
            , Element.Font.color theme.bgPanel
            ]
         , Element.Border.color theme.borderDefault
         , Element.Font.color theme.fontControl
         ]
            ++ extraAttrs
        )
        mainAttrs


text :
    Theme
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder msg)
        , label : Element.Input.Label msg
        }
    -> Element msg
text theme extraAttrs mainAttrs =
    Element.Input.text
        (inputAttrs theme ++ extraAttrs)
        mainAttrs


password :
    Theme
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder msg)
        , label : Element.Input.Label msg
        , show : Bool
        }
    -> Element msg
password theme extraAttrs mainAttrs =
    Element.Input.newPassword
        (inputAttrs theme ++ extraAttrs)
        mainAttrs


multiline :
    Theme
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Element.Input.Placeholder msg)
        , label : Element.Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline theme extraAttrs mainAttrs =
    Element.Input.multiline
        (inputAttrs theme ++ extraAttrs)
        mainAttrs


type Language
    = Enclojure
    | Sql


onCodeMirrorChange : (String -> msg) -> Html.Attribute msg
onCodeMirrorChange tagger =
    Html.Events.on "input" (Decode.map tagger (Decode.at [ "target", "fullValue" ] Decode.string))


code :
    Theme
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , label : Maybe (Element msg)
        , language : Language
        , value : String
        }
    -> Element msg
code theme extraAttrs mainAttrs =
    let
        mode =
            case mainAttrs.language of
                Enclojure ->
                    "clojure"

                Sql ->
                    "sql"
    in
    Element.column
        (inputWidth :: extraAttrs)
        [ mainAttrs.label |> Maybe.withDefault Element.none
        , Element.html <|
            Html.node "code-editor"
                [ Html.Attributes.attribute "initvalue" mainAttrs.value
                , Html.Attributes.attribute "mode" mode
                , Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , onCodeMirrorChange mainAttrs.onChange
                ]
                []
        ]
