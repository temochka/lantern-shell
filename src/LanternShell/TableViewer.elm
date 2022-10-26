module LanternShell.TableViewer exposing (TableViewer, new, render)

import Dict
import Element exposing (Element)
import Element.Font as Font
import Html
import Json.Decode
import Lantern
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternShell.FlexiQuery as FlexiQuery
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias TableViewer =
    { results : List FlexiQuery.Result
    , selected : Maybe ( FlexiQuery.Result, String )
    }


new : List FlexiQuery.Result -> TableViewer
new results =
    { results = results
    , selected = Nothing
    }


monospaceCell : String -> Element msg
monospaceCell text =
    Element.paragraph [ Font.family [ Font.typeface "Fira Mono", Font.typeface "Fira Code", Font.typeface "Monaco", Font.monospace ] ] [ Element.text text ]


render : LanternUi.Theme.Theme -> (TableViewer -> msg) -> TableViewer -> Element msg
render theme setState ({ results, selected } as state) =
    let
        renderValue id val =
            case val of
                Lantern.Query.Null ->
                    monospaceCell "NULL"

                Lantern.Query.Integer i ->
                    monospaceCell <| String.fromInt i

                Lantern.Query.Real r ->
                    monospaceCell <| String.fromFloat r

                Lantern.Query.Text t ->
                    let
                        len =
                            String.length t

                        preview =
                            String.lines t
                                |> List.head
                                |> Maybe.withDefault ""
                                |> (\firstLine ->
                                        if String.length firstLine <= 35 then
                                            firstLine

                                        else
                                            String.slice 0 32 firstLine
                                   )

                        button =
                            if selected == Just id then
                                LanternUi.Input.button theme
                                    [ Element.below
                                        (LanternUi.popup theme
                                            [ Element.padding 10
                                            , Element.width (Element.px 300)
                                            , Element.height (Element.fill |> Element.minimum 200 |> Element.maximum 500)
                                            , Element.scrollbars
                                            ]
                                            (Element.html (Html.pre [] [ Html.text t ]))
                                        )
                                    ]
                                    { onPress = Just <| setState { state | selected = Nothing }
                                    , label = Element.text "x"
                                    }

                            else
                                LanternUi.Input.button theme
                                    []
                                    { onPress = Just <| setState { state | selected = Just id }
                                    , label = Element.text "…"
                                    }
                    in
                    if preview /= t then
                        Element.paragraph []
                            [ Element.text preview
                            , button
                            ]

                    else
                        Element.text t

        columns =
            results
                |> List.head
                |> Maybe.map (Dict.keys >> List.sort)
                |> Maybe.withDefault []
                |> List.map
                    (\title ->
                        { header = LanternUi.boldText title
                        , width = Element.fill
                        , view =
                            \row ->
                                Dict.get title row
                                    |> Maybe.map (renderValue ( row, title ))
                                    |> Maybe.withDefault Element.none
                        }
                    )
    in
    Element.column
        [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.table [ Element.spacingXY 20 20 ]
            { data = results
            , columns = columns
            }
        ]
