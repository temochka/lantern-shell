module DevTools.TableViewer exposing (State(..), TableViewer, init, liveQuery, loadRows, loadTable, render, rowDecoder)

import DevTools.FlexiQuery as FlexiQuery
import Dict
import Html
import Json.Decode
import Lantern
import Lantern.Query


type State
    = Inactive
    | Loading String
    | Loaded String (List FlexiQuery.Result) Int


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


liveQuery : TableViewer -> (Result Lantern.Error ( List FlexiQuery.Result, List Int ) -> msg) -> Maybe (Lantern.LiveQuery msg)
liveQuery { rowsPerPage, page, state } toMsg =
    let
        offset =
            (page - 1) * rowsPerPage

        rowsQuery table =
            Lantern.Query.withArguments
                ("SELECT * FROM "
                    ++ table
                    ++ " LIMIT $limit OFFSET $offset"
                )
                [ ( "$limit", Lantern.Query.Int rowsPerPage )
                , ( "$offset", Lantern.Query.Int offset )
                ]

        countQuery table =
            Lantern.Query.withNoArguments
                ("SELECT COUNT(*) AS 'count' FROM " ++ table)

        query table =
            Lantern.prepareLiveQuery2
                ( rowsQuery table, rowDecoder )
                ( countQuery table, Json.Decode.field "count" Json.Decode.int )
                toMsg
    in
    case state of
        Inactive ->
            Nothing

        Loading table ->
            Just (query table)

        Loaded table _ _ ->
            Just (query table)


loadRows : TableViewer -> List FlexiQuery.Result -> Int -> TableViewer
loadRows tableViewer rows total =
    case tableViewer.state of
        Inactive ->
            tableViewer

        Loading table ->
            { tableViewer | state = Loaded table rows total }

        Loaded table _ _ ->
            { tableViewer | state = Loaded table rows total }


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

        Loaded table results _ ->
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
