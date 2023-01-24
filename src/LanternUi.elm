module LanternUi exposing
    ( boldText
    , columnLayout
    , fullWidthColumn
    , listSpacing
    , loader
    , noneAttribute
    , panel
    , popup
    , text
    , textPanelHeader
    )

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Html.Attributes
import LanternUi.Persistent exposing (Persistent)
import LanternUi.Theme exposing (Theme)


zeroPadding :
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }
zeroPadding =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


listSpacing : Element.Attribute msg
listSpacing =
    Element.spacing 5


panelContentPadding : Element.Attribute msg
panelContentPadding =
    Element.padding 10


panelHeader : Theme -> List (Element.Attribute msg) -> Element msg -> Element msg
panelHeader theme attributes content =
    Element.el
        ([ Element.paddingEach { zeroPadding | bottom = 5 }
         , Element.Font.color theme.fontSecondary
         , Element.Font.size 12
         ]
            ++ attributes
        )
        content


loader : (Maybe id -> a -> Element msg) -> Persistent id a -> Element msg
loader viewFn p =
    p
        |> LanternUi.Persistent.state
        |> Maybe.map (viewFn (LanternUi.Persistent.id p))
        |> Maybe.withDefault (Element.text "Loading...")


type PanelHeader msg
    = TextPanelHeader (List (Element.Attribute msg)) String (List (Element msg))


textPanelHeader : List (Element.Attribute msg) -> String -> List (Element msg) -> PanelHeader msg
textPanelHeader =
    TextPanelHeader


panel : Theme -> List (Element.Attribute msg) -> { content : Element msg, header : Maybe (PanelHeader msg) } -> Element msg
panel theme attributes { content, header } =
    Element.column
        ([ panelContentPadding
         , Element.width Element.fill
         , Element.height Element.fill
         , Element.clip
         , Element.Border.solid
         , Element.Border.width 1
         , Element.Border.rounded 5
         , Element.Border.color theme.borderDefault
         , Element.Background.color theme.bgPanel
         , Element.spacing 10
         ]
            ++ attributes
        )
        [ header
            |> Maybe.map
                (\(TextPanelHeader attrs str addOns) ->
                    Element.row [ Element.width Element.fill ]
                        (panelHeader theme attrs (Element.text str) :: addOns)
                )
            |> Maybe.withDefault Element.none
        , content
        ]


popup : Theme -> List (Element.Attribute msg) -> Element msg -> Element msg
popup theme attrs content =
    Element.el
        ([ Element.Background.color theme.bgDefault
         , Element.Border.color theme.borderDefault
         , Element.Border.width 1
         , Element.Font.color theme.fontDefault
         ]
            ++ attrs
        )
        content


columnLayout theme attributes elements =
    Element.column
        ([ Element.spacing 5
         , Element.width Element.fill
         , Element.height Element.fill
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


text : String -> Element msg
text str =
    Element.el [ Element.Font.family [ Element.Font.typeface "SF Pro Display", Element.Font.sansSerif ] ] (Element.text str)


boldText : String -> Element msg
boldText str =
    Element.el [ Element.Font.bold ] (text str)
