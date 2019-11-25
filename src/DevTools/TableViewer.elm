module DevTools.TableViewer exposing (State(..), TableViewer, init, loadRows, loadTable, render, rowDecoder, rowsQuery)

import DevTools.FlexiQuery as FlexiQuery
import Dict
import Html
import Lantern.Query


type State
    = Inactive
    | Loading String
    | Loaded String (List FlexiQuery.Result)


type alias TableViewer =
    { rowsPerPage : Int
    , page : Int
    , state : State
    }


init : TableViewer
init =
    { rowsPerPage = 10
    , page = 1
    , state = Inactive
    }


rowDecoder =
    FlexiQuery.resultDecoder


rowsQuery : TableViewer -> Lantern.Query.Query
rowsQuery { rowsPerPage, page, state } =
    let
        offset =
            (page - 1) * rowsPerPage

        query table =
            Lantern.Query.withArguments
                ("SELECT * FROM "
                    ++ table
                    ++ " LIMIT "
                    ++ String.fromInt offset
                    ++ ","
                    ++ String.fromInt rowsPerPage
                )
                []
    in
    case state of
        Inactive ->
            Lantern.Query.withNoArguments "SELECT 1"

        Loading table ->
            query table

        Loaded table _ ->
            query table


loadRows : TableViewer -> List FlexiQuery.Result -> TableViewer
loadRows tableViewer rows =
    case tableViewer.state of
        Inactive ->
            tableViewer

        Loading table ->
            { tableViewer | state = Loaded table rows }

        Loaded table _ ->
            { tableViewer | state = Loaded table rows }


loadTable : TableViewer -> String -> TableViewer
loadTable tableViewer table =
    { tableViewer | state = Loading table, page = 1 }


render : TableViewer -> Html.Html msg
render { rowsPerPage, state } =
    case state of
        Inactive ->
            Html.div [] []

        Loading table ->
            Html.div [] [ Html.text <| "loading " ++ table ]

        Loaded table results ->
            let
                titles =
                    results |> List.head |> Maybe.map (Dict.keys >> List.sort) |> Maybe.withDefault []

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

                row result =
                    titles
                        |> List.map ((\t -> Dict.get t result) >> Maybe.map valueToString >> Maybe.withDefault "" >> (\s -> Html.td [] [ Html.text s ]))
                        |> Html.tr []
            in
            Html.table []
                [ Html.caption [] [ Html.text table ]
                , Html.thead []
                    [ Html.tr [] (List.map (\t -> Html.th [] [ Html.text t ]) titles) ]
                , Html.tbody [] (List.map row results)
                ]
