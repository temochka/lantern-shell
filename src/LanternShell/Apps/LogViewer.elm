module LanternShell.Apps.LogViewer exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Font
import Html
import Lantern.App
import Lantern.Log exposing (Log)
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { log : Log, theme : LanternUi.Theme.Theme }


type alias Model =
    { inspectedLine : Maybe Int }


type Message
    = InspectLine Int
    | CloseInspector


init : Model
init =
    { inspectedLine = Nothing }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        InspectLine id ->
            ( { model | inspectedLine = Just id }, Cmd.none )

        CloseInspector ->
            ( { model | inspectedLine = Nothing }, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { log, theme } model =
    LanternUi.columnLayout
        theme
        []
        [ log
            |> .lines
            |> List.map
                (\{ id, text, payload } ->
                    let
                        button =
                            case payload of
                                Just p ->
                                    if Just id == model.inspectedLine then
                                        LanternUi.Input.button theme
                                            [ Element.below
                                                (LanternUi.popup theme
                                                    [ Element.width (Element.px 500)
                                                    , Element.height (Element.fill |> Element.minimum 400 |> Element.maximum 800)
                                                    , Element.scrollbars
                                                    , Element.padding 5
                                                    ]
                                                    (Element.html (Html.pre [] [ Html.text p ]))
                                                )
                                            ]
                                            { label = Element.text "Close", onPress = Just <| Lantern.App.Message <| CloseInspector }

                                    else
                                        LanternUi.Input.button theme [] { label = Element.text "Inspect", onPress = Just <| Lantern.App.Message <| InspectLine id }

                                Nothing ->
                                    Element.none
                    in
                    Element.paragraph [ Element.width Element.fill, Element.spacing 10 ]
                        [ Element.text text
                        , Element.text " "
                        , button
                        ]
                )
            |> Element.column
                [ Element.padding 10
                , Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarX
                , Element.spacing 20
                ]
        ]


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Logs"
        , init = init
        , view = view
        , update = update
        }
