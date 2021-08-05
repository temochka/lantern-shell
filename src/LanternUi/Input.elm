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
        ([ Element.Background.color theme.bgActive
         , Element.padding 7
         , Element.Border.rounded 3
         , Element.Font.color theme.fontContrast
         , Element.Font.size 14
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
        ([ inputWidth ] ++ extraAttrs)
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
        ([ inputWidth ] ++ extraAttrs)
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
        ([ inputWidth ] ++ extraAttrs)
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
        , value : String
        , language : Language
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
    Element.el (inputWidth :: extraAttrs)
        (Element.html <|
            Html.node "code-editor"
                [ Html.Attributes.attribute "initvalue" mainAttrs.value
                , Html.Attributes.attribute "mode" mode
                , onCodeMirrorChange mainAttrs.onChange
                ]
                []
        )
