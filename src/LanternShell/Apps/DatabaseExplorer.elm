module LanternShell.Apps.DatabaseExplorer exposing (Message, Model, init, lanternApp, liveQueries, update, view)

import Dict exposing (Dict)
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


type alias Model =
    { tables : List Table
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
    , tableViewer = TableViewer.init
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        LoadTable table ->
            let
                newTableViewerState =
                    TableViewer.loadTable model.tableViewer table

                newState =
                    { model | tableViewer = newTableViewerState }

                _ =
                    Debug.log "newState" newState
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


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } { tables, tableViewer } =
    let
        tableList =
            tables
                |> List.map
                    (\{ name } ->
                        Element.Input.button
                            ((if TableViewer.loadedTable tableViewer == Just name then
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
                , Element.Border.widthEach { top = 0, left = 0, right = 1, bottom = 0 }
                , Element.Border.color theme.borderDefault
                , LanternUi.listSpacing
                ]
                (LanternUi.boldText "Tables" :: tableList)
            , Element.column
                [ Element.width (Element.fillPortion 5), Element.alignTop, LanternUi.listSpacing ]
                [ LanternUi.boldText "Contents"
                , TableViewer.render tableViewer
                ]
            ]
        ]


liveQueries : Model -> List (LiveQuery Message)
liveQueries { tableViewer } =
    let
        tablesQuery =
            Lantern.LiveQuery.prepare ( Lantern.Query.Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" Dict.empty, tableDecoder ) UpdateTables

        tableViewerQueries =
            TableViewer.liveQueries tableViewer UpdateTableRows
    in
    [ tablesQuery ] ++ tableViewerQueries


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
