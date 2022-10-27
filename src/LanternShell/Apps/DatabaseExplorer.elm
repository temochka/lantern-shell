module LanternShell.Apps.DatabaseExplorer exposing (Message, Model, init, lanternApp, liveQueries, update, view)

import Dict
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Json.Decode
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternShell.FlexiQuery as FlexiQuery
import LanternShell.TableViewer as TableViewer
import LanternUi
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type Message
    = LoadTable String
    | UpdateTableRows (Result Lantern.Error ( List FlexiQuery.Result, List Int ))
    | UpdateTables (Result Lantern.Error (List Table))
    | UpdateTableViewer TableViewer.TableViewer


type CurrentTable
    = Inactive
    | Loading { name : String }
    | Loaded { name : String, viewer : TableViewer.TableViewer, totalRows : Int }


type alias Model =
    { tables : List Table
    , currentTable : CurrentTable
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
    , currentTable = Inactive
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        LoadTable table ->
            ( { model | currentTable = Loading { name = table } }
            , Cmd.none
            )

        UpdateTables result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok tables ->
                    ( { model | tables = tables }, Cmd.none )

        UpdateTableRows result ->
            case result of
                Err _ ->
                    ( model, Cmd.none )

                Ok ( rows, count ) ->
                    model.currentTable
                        |> selectedTableName
                        |> Maybe.map
                            (\tableName ->
                                let
                                    newCurrentTable =
                                        { viewer = TableViewer.new rows
                                        , totalRows = count |> List.head |> Maybe.withDefault 0
                                        , name = tableName
                                        }
                                in
                                ( { model | currentTable = Loaded newCurrentTable }, Cmd.none )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )

        UpdateTableViewer viewer ->
            case model.currentTable of
                Loaded currentTable ->
                    let
                        newCurrentTable =
                            { currentTable | viewer = viewer }
                    in
                    ( { model | currentTable = Loaded newCurrentTable }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


selectedTableName : CurrentTable -> Maybe String
selectedTableName currentTable =
    case currentTable of
        Inactive ->
            Nothing

        Loaded { name } ->
            Just name

        Loading { name } ->
            Just name


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } { tables, currentTable } =
    let
        tableList =
            tables
                |> List.map
                    (\{ name } ->
                        Element.Input.button
                            ((if selectedTableName currentTable == Just name then
                                [ Element.Background.color theme.bgActive, Element.Font.color theme.fontContrast ]

                              else
                                [ LanternUi.noneAttribute ]
                             )
                                ++ [ Element.width Element.fill, Element.padding 5 ]
                            )
                            { label = LanternUi.text name, onPress = Just (Lantern.App.Message (LoadTable name)) }
                    )
    in
    LanternUi.columnLayout
        theme
        []
        [ Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.column
                [ Element.alignTop
                , Element.width (Element.fillPortion 2)
                , Element.height Element.fill
                , Element.Border.widthEach { top = 0, left = 0, right = 1, bottom = 0 }
                , Element.Border.color theme.borderDefault
                , LanternUi.listSpacing
                ]
                (LanternUi.boldText "Tables" :: tableList)
            , Element.column
                [ Element.width (Element.fillPortion 5), Element.alignTop, LanternUi.listSpacing, Element.height Element.fill, Element.scrollbarX ]
                [ LanternUi.boldText "Contents"
                , case currentTable of
                    Inactive ->
                        Element.none

                    Loading _ ->
                        Element.text "Loading..."

                    Loaded { viewer } ->
                        TableViewer.render theme (UpdateTableViewer >> Lantern.App.Message) viewer
                ]
            ]
        ]


tableViewerQueries : Model -> List (LiveQuery Message)
tableViewerQueries model =
    let
        page =
            1

        rowsPerPage =
            50

        offset =
            (page - 1) * rowsPerPage

        rowsQuery table =
            Lantern.Query.withArguments
                ("SELECT * FROM "
                    ++ table
                    ++ " LIMIT $limit OFFSET $offset"
                )
                [ ( "$limit", Lantern.Query.Integer rowsPerPage )
                , ( "$offset", Lantern.Query.Integer offset )
                ]

        countQuery table =
            Lantern.Query.withNoArguments
                ("SELECT COUNT(*) AS 'count' FROM " ++ table)

        query table =
            Lantern.LiveQuery.prepare2
                ( rowsQuery table, FlexiQuery.resultDecoder )
                ( countQuery table, Json.Decode.field "count" Json.Decode.int )
                UpdateTableRows
    in
    model.currentTable
        |> selectedTableName
        |> Maybe.map (query >> List.singleton)
        |> Maybe.withDefault []


liveQueries : Model -> List (LiveQuery Message)
liveQueries model =
    let
        tablesQuery =
            Lantern.LiveQuery.prepare ( Lantern.Query.Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" Dict.empty, tableDecoder ) UpdateTables
    in
    [ tablesQuery ] ++ tableViewerQueries model


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.liveApp
        { name = "Database Explorer"
        , init = init
        , view = view
        , update = update
        , liveQueries = liveQueries
        , subscriptions = always Sub.none
        }
