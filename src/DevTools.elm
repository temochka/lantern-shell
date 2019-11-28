port module DevTools exposing (..)

import Browser
import Debug
import DevTools.ArgumentParser as ArgumentParser
import DevTools.FlexiQuery as FlexiQuery
import DevTools.TableViewer as TableViewer
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Lantern
import Lantern.Encoders
import Lantern.Log
import Lantern.Query
import Lantern.Request
import String


port lanternRequestPort : Lantern.RequestPort msg


port lanternResponsePort : Lantern.ResponsePort msg



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Table =
    { name : String }


type alias LiveQueries =
    { databaseTables : Lantern.Query.Query
    , tableViewerRows : Lantern.Query.Query
    }


type alias LiveResults =
    { databaseTables : List Table
    , tableViewerRows : List FlexiQuery.Result
    }


tableDecoder : Json.Decode.Decoder Table
tableDecoder =
    Json.Decode.map
        Table
        (Json.Decode.field "name" Json.Decode.string)


type alias Model =
    { readerQuery : String
    , readerQueryArguments : Dict String String
    , writerQuery : String
    , writerQueryArguments : Dict String String
    , queryResult : List FlexiQuery.Result
    , queryError : Maybe Lantern.Error
    , liveQueries : LiveQueries
    , liveResults : LiveResults
    , tableViewer : TableViewer.TableViewer
    , ddl : String
    , ddlResult : Bool
    , ddlError : Maybe Lantern.Error
    , ping : String
    , pong : String
    , serverResponse : Maybe String
    , lanternConnection : Lantern.Connection Msg
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        tableViewer =
            TableViewer.init

        liveQueries =
            { databaseTables = Lantern.Query.Query "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name" Dict.empty
            , tableViewerRows = TableViewer.rowsQuery tableViewer
            }

        lanternConnection =
            Lantern.newConnection lanternRequestPort lanternResponsePort LanternMessage

        lanternCmd =
            Lantern.liveQuery2
                ( liveQueries.databaseTables, tableDecoder )
                ( liveQueries.tableViewerRows, TableViewer.rowDecoder )
                LiveResults
                UpdateLiveResults
                lanternConnection
    in
    ( { readerQuery = ""
      , readerQueryArguments = Dict.empty
      , writerQuery = ""
      , writerQueryArguments = Dict.empty
      , queryResult = []
      , queryError = Nothing
      , liveQueries = liveQueries
      , liveResults =
            { databaseTables = [], tableViewerRows = [] }
      , tableViewer = tableViewer
      , ddl = ""
      , ddlResult = False
      , ddlError = Nothing
      , ping = ""
      , pong = ""
      , serverResponse = Nothing
      , lanternConnection = lanternConnection
      }
    , lanternCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Lantern.subscriptions model.lanternConnection



-- UPDATE


type Msg
    = UpdateReaderQuery String
    | UpdateReaderQueryArgument String String
    | UpdateWriterQuery String
    | UpdateWriterQueryArgument String String
    | UpdatePing String
    | UpdateDdl String
    | UpdateLiveResults (Result Lantern.Error LiveResults)
    | RunReaderQuery
    | RunWriterQuery
    | RunPing
    | RunDdl
    | DdlResult Bool
    | ReceivePong String
    | ReaderQueryResult (Result Lantern.Error (List FlexiQuery.Result))
    | WriterQueryResult Bool
    | LanternMessage (Lantern.Message Msg)
    | LoadTable String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateReaderQuery query ->
            let
                argumentNames =
                    ArgumentParser.parse query

                arguments =
                    argumentNames
                        |> List.map (\n -> ( n, Dict.get n model.readerQueryArguments |> Maybe.withDefault "" ))
                        |> Dict.fromList
            in
            ( { model | readerQuery = query, readerQueryArguments = arguments }, Cmd.none )

        UpdateReaderQueryArgument name value ->
            ( { model | readerQueryArguments = Dict.insert name value model.readerQueryArguments }, Cmd.none )

        UpdateWriterQuery query ->
            let
                argumentNames =
                    ArgumentParser.parse query

                arguments =
                    argumentNames
                        |> List.map (\n -> ( n, Dict.get n model.writerQueryArguments |> Maybe.withDefault "" ))
                        |> Dict.fromList
            in
            ( { model | writerQuery = query, writerQueryArguments = arguments }, Cmd.none )

        UpdateWriterQueryArgument name value ->
            ( { model | writerQueryArguments = Dict.insert name value model.writerQueryArguments }, Cmd.none )

        UpdatePing ping ->
            ( { model | ping = ping }, Cmd.none )

        UpdateDdl ddl ->
            ( { model | ddl = ddl }, Cmd.none )

        UpdateLiveResults result ->
            case result of
                Err e ->
                    ( { model | queryError = Just e }, Cmd.none )

                Ok liveResults ->
                    ( { model | liveResults = liveResults, tableViewer = TableViewer.loadRows model.tableViewer liveResults.tableViewerRows }, Cmd.none )

        RunPing ->
            ( model, Lantern.echo model.ping ReceivePong model.lanternConnection )

        ReceivePong pong ->
            ( { model | pong = pong }, Cmd.none )

        RunReaderQuery ->
            let
                query =
                    { source = model.readerQuery
                    , arguments = Dict.map (\_ v -> Lantern.Query.String v) model.readerQueryArguments
                    }
            in
            ( model
            , Lantern.readerQuery
                query
                FlexiQuery.resultDecoder
                ReaderQueryResult
                model.lanternConnection
            )

        RunWriterQuery ->
            let
                query =
                    { source = model.writerQuery
                    , arguments = Dict.map (\_ v -> Lantern.Query.String v) model.writerQueryArguments
                    }
            in
            ( model
            , Lantern.writerQuery
                query
                WriterQueryResult
                model.lanternConnection
            )

        RunDdl ->
            let
                ddlQuery =
                    Lantern.Query.withNoArguments model.ddl
            in
            ( model, Lantern.migrate ddlQuery DdlResult model.lanternConnection )

        DdlResult r ->
            ( { model | ddlResult = r }, Cmd.none )

        ReaderQueryResult result ->
            case result of
                Err error ->
                    ( { model | queryError = Just error }, Cmd.none )

                Ok queryResult ->
                    ( { model | queryResult = queryResult }, Cmd.none )

        WriterQueryResult _ ->
            ( model, Cmd.none )

        LanternMessage message ->
            let
                ( lanternConnection, lanternCmd ) =
                    Lantern.update message model.lanternConnection
            in
            ( { model | lanternConnection = lanternConnection }, lanternCmd )

        LoadTable table ->
            let
                newTableViewerState =
                    TableViewer.loadTable model.tableViewer table

                liveQueries =
                    model.liveQueries

                newLiveQueries =
                    { liveQueries | tableViewerRows = TableViewer.rowsQuery newTableViewerState }
            in
            ( { model | tableViewer = newTableViewerState, liveQueries = newLiveQueries }
            , Lantern.liveQuery2
                ( newLiveQueries.databaseTables, tableDecoder )
                ( newLiveQueries.tableViewerRows, TableViewer.rowDecoder )
                LiveResults
                UpdateLiveResults
                model.lanternConnection
            )



-- VIEW


logView : Lantern.Log.Log -> Html Msg
logView log =
    Html.ul []
        (log.lines
            |> List.map (\( _, line ) -> Html.li [] [ text line ])
        )


resultsTable : List FlexiQuery.Result -> Html Msg
resultsTable results =
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
                |> List.map ((\t -> Dict.get t result) >> Maybe.map valueToString >> Maybe.withDefault "" >> (\s -> Html.td [] [ text s ]))
                |> Html.tr []
    in
    Html.table []
        [ Html.thead []
            [ Html.tr [] (List.map (\t -> Html.th [] [ text t ]) titles) ]
        , Html.tbody [] (List.map row results)
        ]


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ Html.Events.onSubmit RunReaderQuery ]
            [ div [] [ Html.textarea [ onInput UpdateReaderQuery, Html.Attributes.cols 80 ] [] ]
            , div []
                (model.readerQueryArguments
                    |> Dict.toList
                    |> List.map
                        (\( name, value ) ->
                            Html.label [] [ text (name ++ ": "), input [ Html.Attributes.type_ "text", onInput (UpdateReaderQueryArgument name), Html.Attributes.value value ] [] ]
                        )
                )
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run reader query" ] [] ]
            , resultsTable model.queryResult
            , div [] [ text ("Server error: " ++ (model.queryError |> Maybe.map Lantern.errorToString |> Maybe.withDefault "")) ]
            ]
        , Html.form [ Html.Events.onSubmit RunWriterQuery ]
            [ div [] [ Html.textarea [ onInput UpdateWriterQuery, Html.Attributes.cols 80 ] [] ]
            , div []
                (model.writerQueryArguments
                    |> Dict.toList
                    |> List.map
                        (\( name, value ) ->
                            Html.label [] [ text (name ++ ": "), input [ Html.Attributes.type_ "text", onInput (UpdateWriterQueryArgument name), Html.Attributes.value value ] [] ]
                        )
                )
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run writer query" ] [] ]
            ]
        , Html.form [ Html.Events.onSubmit RunDdl ]
            [ div [] [ Html.textarea [ onInput UpdateDdl, Html.Attributes.cols 80 ] [] ]
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run DDL" ] [] ]
            , div [] [ text ("Result: " ++ Debug.toString model.ddlResult) ]
            , div [] [ text "Tables:", Html.ul [] (List.map (\{ name } -> Html.li [] [ Html.a [ Html.Attributes.href "#", onClick (LoadTable name) ] [ Html.text name ] ]) model.liveResults.databaseTables) ]
            , TableViewer.render model.tableViewer
            ]
        , Html.form [ Html.Events.onSubmit RunPing ]
            [ div [] [ Html.textarea [ onInput UpdatePing, Html.Attributes.cols 80 ] [] ]
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run echo" ] [] ]
            , div [] [ text ("Results: " ++ Debug.toString model.pong) ]
            ]
        , logView (Lantern.log model.lanternConnection)
        ]
