module LanternUi exposing (listSpacing, panel)

import Element exposing (Element)
import LanternUi.Theme exposing (Theme)


listSpacing : Element.Attribute msg
listSpacing =
    Element.spacing 5


panelContentSpacing : Element.Attribute msg
panelContentSpacing =
    Element.spacing 5


panelContentPadding : Element.Attribute msg
panelContentPadding =
    Element.padding 5


panel : Theme -> List (Element.Attribute msg) -> List (Element msg) -> Element msg
panel theme attributes elements =
    Element.column
        ([ panelContentSpacing, panelContentPadding, Element.width Element.fill, Element.scrollbarX ] ++ attributes)
        elements
