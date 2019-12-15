module LanternUi.Theme exposing (Theme, lightTheme)

import Element


type alias Theme =
    { bgDefault : Element.Color
    , bgHighlight : Element.Color
    , controlActive : Element.Color
    }


lightTheme =
    { bgDefault = Element.rgb255 0xFF 0xFF 0xFF
    , bgHighlight = Element.rgb255 0xE2 0xE2 0xE2
    , controlActive = Element.rgb255 0x77 0x77 0x77
    }
