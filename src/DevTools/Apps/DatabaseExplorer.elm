module DevTools.Apps.DatabaseExplorer exposing (Message, Model, init, lanternApp, update, view)

import DevTools.FlexiQuery as FlexiQuery
import DevTools.TableViewer as TableViewer
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Json.Decode
import Lantern
import Lantern.Query
import LanternUi
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type Message
    = LoadTable String
    | UpdateTableRows (Result Lantern.Error ( List FlexiQuery.Result, List Int ))
    | UpdateTables (Result Lantern.Error (List Table))


type alias Model =
    { tables : List Table
    , rows : List FlexiQuery.Result
    , rowsCount : Int
    , tableViewer : TableViewer.TableViewer
    }


type alias Table =
    { name : String }


tableDecoder : Json.Decode.Decoder Table
tableDecoder =
    Json.Decode.map
        Table
        (Json.Decode.field "name" Json.Decode.string)


init : Model
init =
    { tables = []
    , rows = []
    , rowsCount = 0
    , tableViewer = TableViewer.init
    }


update : Context -> Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
update _ msg model =
    case msg of
        LoadTable table ->
            let
                newTableViewerState =
                    TableViewer.loadTable model.tableViewer table

                newState =
                    { model | tableViewer = newTableViewerState }
            in
            ( newState
            , Cmd.none
            )

        UpdateTables result ->
            case result of
                Err err ->
                    Debug.todo "implement error handling"

                Ok tables ->
                    ( { model | tables = tables }, Cmd.none )

        UpdateTableRows result ->
            case result of
                Err err ->
                    Debug.todo "implement error handling"

                Ok ( rows, count ) ->
                    let
                        newTableViewer =
                            TableViewer.loadRows
                                model.tableViewer
                                rows
                                (count |> List.head |> Maybe.withDefault 0)
                    in
                    ( { model | tableViewer = newTableViewer }, Cmd.none )


view : Context -> Model -> Element (Lantern.Message Message)
view { theme } { tables, tableViewer } =
    let
        tableList =
            tables
                |> List.map (\{ name } -> Element.Input.button [] { label = Element.text name, onPress = Just (Lantern.AppMessage (LoadTable name)) })
    in
    LanternUi.columnLayout
        theme
        []
        [ Element.row [ Element.width Element.fill ]
            [ Element.column [ Element.width (Element.fillPortion 2) ] tableList
            , Element.el [ Element.width (Element.fillPortion 5) ] (TableViewer.render tableViewer)
            ]
        ]


liveQueries : Context -> Model -> List (Lantern.LiveQuery Message)
liveQueries _ { tableViewer } =
    let
        tablesQuery =
            Lantern.prepareLiveQuery ( Lantern.Query.Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" Dict.empty, tableDecoder ) UpdateTables

        tableViewerQueries =
            TableViewer.liveQueries tableViewer UpdateTableRows
    in
    [ tablesQuery ] ++ tableViewerQueries


lanternApp : Lantern.App Context Model Message
lanternApp =
    Lantern.liveApp
        { model = init
        , view = view
        , update = update
        , liveQueries = liveQueries
        }
