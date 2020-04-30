module LanternUi.Theme exposing (Theme, lightTheme)

import Element


type alias Theme =
    { bgDefault : Element.Color
    , bgContrast : Element.Color
    , bgPanel : Element.Color
    , bgHighlight : Element.Color
    , bgActive : Element.Color
    , bgInactive : Element.Color
    , borderDefault : Element.Color
    , fontDefault : Element.Color
    , fontSecondary : Element.Color
    , fontContrast : Element.Color
    , fontContrastInactive : Element.Color
    , borderHighlight : Element.Color
    , panelShadow : Element.Color
    }


lightTheme : Theme
lightTheme =
    { bgDefault = Element.rgb255 0xF3 0xF3 0xF3
    , bgContrast = Element.rgb255 0x2C 0x2C 0x2C
    , bgPanel = Element.rgb255 0xFF 0xFF 0xFF
    , bgHighlight = Element.rgb255 214 235 235
    , bgActive = Element.rgb255 2 116 232
    , bgInactive = Element.rgb255 146 195 227
    , borderDefault = Element.rgb255 207 207 207
    , fontDefault = Element.rgb255 0 0 0
    , fontSecondary = Element.rgb255 128 128 128
    , fontContrast = Element.rgb 255 255 255
    , fontContrastInactive = Element.rgb255 128 128 128
    , borderHighlight = Element.rgb255 153 202 235
    , panelShadow = Element.rgb 0x00 0x00 0xFF
    }
