module DevTools.Ui.StatusBar exposing (Message, StatusBar, new, render, update)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)


type Message
    = ToggleWindow


type alias StatusBar =
    { showWindow : Bool
    }


type alias ToMsg msg =
    Message -> msg


new : StatusBar
new =
    { showWindow = True }


statusBar : StatusBar -> ToMsg msg -> Element msg
statusBar _ toMsg =
    Element.row [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] (Input.button [] { onPress = Just (toMsg ToggleWindow), label = Element.text "Lantern" }) ]


window : StatusBar -> ToMsg msg -> Element msg -> Element msg
window options toMsg content =
    Element.el
        [ Element.padding 100
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        ]
        (Element.el
            [ Border.color (Element.rgb 0 0 0)
            , Border.width 1
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.centerX
            , Element.centerY
            ]
            content
        )


render : StatusBar -> ToMsg msg -> Element msg -> Html msg
render options toMsg content =
    let
        windowContent =
            if options.showWindow then
                window options toMsg content

            else
                Element.none
    in
    Element.layout
        [ Element.inFront windowContent
        , Element.inFront (statusBar options toMsg)
        ]
        Element.none


update : Message -> StatusBar -> ( StatusBar, Cmd msg )
update msg options =
    case msg of
        ToggleWindow ->
            ( { options | showWindow = not options.showWindow }, Cmd.none )
