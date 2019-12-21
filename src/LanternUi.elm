module LanternUi exposing (fullWidthColumn, listSpacing, noneAttribute, panel)

import Element exposing (Element)
import Element.Border
import Html.Attributes
import LanternUi.Theme exposing (Theme)


listSpacing : Element.Attribute msg
listSpacing =
    Element.spacing 5


panelContentSpacing : Element.Attribute msg
panelContentSpacing =
    Element.spacing 5


panelContentPadding : Element.Attribute msg
panelContentPadding =
    Element.padding 10


panel : Theme -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
panel theme attributes elements =
    Element.column
        ([ panelContentSpacing
         , panelContentPadding
         , Element.width Element.fill
         , Element.height Element.fill
         , Element.clip
         , Element.Border.solid
         , Element.Border.width 1
         , Element.Border.color theme.borderColor
         ]
            ++ attributes
        )
        elements


fullWidthColumn : List (Element.Attribute msg) -> List (Element msg) -> Element msg
fullWidthColumn attributes elements =
    Element.column
        ([ Element.width Element.fill ] ++ attributes)
        elements


noneAttribute : Element.Attribute msg
noneAttribute =
    Element.htmlAttribute (Html.Attributes.classList [])
