module LanternUi.Theme exposing (Theme, solarizedDark)

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
    , fontControl : Element.Color
    , fontContrast : Element.Color
    , fontContrastInactive : Element.Color
    , borderHighlight : Element.Color
    }


brblack : Element.Color
brblack =
    Element.rgb255 0 43 54


black : Element.Color
black =
    Element.rgb255 0x07 0x36 0x42


brgreen : Element.Color
brgreen =
    Element.rgb255 88 110 117



-- bryellow : Element.Color
-- bryellow =
--     Element.rgb255 101 123 131
-- brblue : Element.Color
-- brblue =
--     Element.rgb255 131 148 150


brcyan : Element.Color
brcyan =
    Element.rgb255 147 161 161


white : Element.Color
white =
    Element.rgb255 238 232 213


brwhite : Element.Color
brwhite =
    Element.rgb255 253 246 227



-- yellow : Element.Color
-- yellow =
--     Element.rgb255 181 137 0
-- brred : Element.Color
-- brred =
--     Element.rgb255 203 75 22
-- red : Element.Color
-- red =
--     Element.rgb255 220 50 47
-- magenta : Element.Color
-- magenta =
--     Element.rgb255 211 54 130


brmagenta : Element.Color
brmagenta =
    Element.rgb255 108 113 196


blue : Element.Color
blue =
    Element.rgb255 38 139 210


cyan : Element.Color
cyan =
    Element.rgb255 0x2A 0xA1 0x98



-- green : Element.Color
-- green =
--     Element.rgb255 133 153 0


solarizedDark : Theme
solarizedDark =
    { bgDefault = black
    , bgContrast = white
    , bgPanel = brblack
    , bgHighlight = cyan
    , bgActive = blue
    , bgInactive = brblack
    , borderDefault = cyan
    , fontDefault = brwhite
    , fontSecondary = white
    , fontControl = cyan
    , fontContrast = brgreen
    , fontContrastInactive = brcyan
    , borderHighlight = brmagenta
    }
