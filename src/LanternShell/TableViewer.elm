module LanternShell.TableViewer exposing (State(..), TableViewer, init, liveQueries, loadRows, loadTable, loadedTable, render, rowDecoder)

import Dict
import Element exposing (Element)
import Json.Decode
import Lantern
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternShell.FlexiQuery as FlexiQuery
import LanternUi


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


liveQueries : TableViewer -> (Result Lantern.Error ( List FlexiQuery.Result, List Int ) -> msg) -> List (LiveQuery msg)
liveQueries { rowsPerPage, page, state } toMsg =
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
            Lantern.LiveQuery.prepare2
                ( rowsQuery table, rowDecoder )
                ( countQuery table, Json.Decode.field "count" Json.Decode.int )
                toMsg
    in
    case state of
        Inactive ->
            []

        Loading table ->
            [ query table ]

        Loaded table _ _ ->
            [ query table ]


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


loadedTable : TableViewer -> Maybe String
loadedTable tableViewer =
    case tableViewer.state of
        Inactive ->
            Nothing

        Loading table ->
            Just table

        Loaded table _ _ ->
            Just table


render : TableViewer -> Element msg
render { rowsPerPage, state } =
    case state of
        Inactive ->
            Element.none

        Loading table ->
            Element.el [] (Element.text <| "loading" ++ table)

        Loaded table results _ ->
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
                                { header = LanternUi.boldText title
                                , width = Element.fill
                                , view = \row -> Dict.get title row |> Maybe.map valueToString |> Maybe.withDefault "" |> Element.text
                                }
                            )
            in
            Element.column
                [ Element.width Element.fill, Element.spacing 10 ]
                [ Element.text table
                , Element.table [ Element.spacing 5 ]
                    { data = results
                    , columns = columns
                    }
                ]
