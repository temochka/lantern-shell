module LanternUi.Input exposing (button, multiline, text)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
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
