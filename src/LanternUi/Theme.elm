module LanternUi.Theme exposing (Theme, lightTheme)

import Element


type alias Theme =
    { bgDefault : Element.Color
    , bgHighlight : Element.Color
    , panelShadow : Element.Color
    , controlActive : Element.Color
    , borderColor : Element.Color
    }


lightTheme =
    { bgDefault = Element.rgb255 0xFF 0xFF 0xFF
    , bgHighlight = Element.rgb255 0xE2 0xE2 0xE2
    , panelShadow = Element.rgb 0x00 0x00 0xFF
    , controlActive = Element.rgb255 0x77 0x77 0x77
    , borderColor = Element.rgb255 0xE2 0xE2 0xE2
    }
