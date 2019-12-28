module DevTools.Ui.ResultsTable exposing (render)

import DevTools.FlexiQuery as FlexiQuery
import Dict
import Element exposing (Element)
import Lantern.Query


render : List FlexiQuery.Result -> Element msg
render results =
    let
        valueToString val =
            case val of
                Lantern.Query.Null ->
                    ""

                Lantern.Query.Integer i ->
                    String.fromInt i

                Lantern.Query.Real r ->
                    String.fromFloat r

                Lantern.Query.Text t ->
                    t

        columns =
            results
                |> List.head
                |> Maybe.map (Dict.keys >> List.sort)
                |> Maybe.withDefault []
                |> List.map
                    (\title ->
                        { header = Element.text title
                        , width = Element.fill
                        , view = \row -> Dict.get title row |> Maybe.map valueToString |> Maybe.withDefault "" |> Element.text
                        }
                    )
    in
    Element.table []
        { data = results
        , columns = columns
        }
