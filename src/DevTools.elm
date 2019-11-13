port module DevTools exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Lantern
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


type alias EditorQueryResult =
    ( Int, Int )


type Queries
    = EditorQuery (List EditorQueryResult)
    | TablesQuery (List Table)


editorQueryResultDecoder : Json.Decode.Decoder EditorQueryResult
editorQueryResultDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "1" Json.Decode.int)
        (Json.Decode.field "2" Json.Decode.int)


tableDecoder : Json.Decode.Decoder Table
tableDecoder =
    Json.Decode.map
        Table
        (Json.Decode.field "name" Json.Decode.string)


type alias Model =
    { query : String
    , queryResult : List EditorQueryResult
    , queryError : Maybe Lantern.Error
    , ddl : String
    , ddlResult : Bool
    , ddlError : Maybe Lantern.Error
    , ping : String
    , pong : String
    , serverResponse : Maybe String
    , tables : List Table
    , lanternState : Lantern.State Queries Msg
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( lanternState, lanternCmd ) =
            Lantern.init lanternRequestPort lanternResponsePort LanternMessage
                |> Lantern.andThen
                    (Lantern.liveQuery
                        (Lantern.Query.withNoArguments "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
                        (tableDecoder |> Json.Decode.list |> Json.Decode.map TablesQuery)
                        QueryResult
                    )
    in
    ( { query = ""
      , queryResult = []
      , queryError = Nothing
      , ddl = ""
      , ddlResult = False
      , ddlError = Nothing
      , ping = ""
      , pong = ""
      , serverResponse = Nothing
      , tables = []
      , lanternState = lanternState
      }
    , lanternCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Lantern.subscriptions model.lanternState
        ]



-- UPDATE


type Msg
    = UpdateQuery String
    | UpdatePing String
    | UpdateDdl String
    | RunQuery
    | RunPing
    | RunDdl
    | DdlResult Bool
    | ReceivePong String
    | QueryResult (Result Lantern.Error Queries)
    | LanternMessage Lantern.Message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        UpdatePing ping ->
            ( { model | ping = ping }, Cmd.none )

        UpdateDdl ddl ->
            ( { model | ddl = ddl }, Cmd.none )

        RunPing ->
            let
                ( lanternState, cmd ) =
                    Lantern.echo model.lanternState model.ping ReceivePong
            in
            ( { model | lanternState = lanternState }, cmd )

        ReceivePong pong ->
            ( { model | pong = pong }, Cmd.none )

        RunQuery ->
            let
                query =
                    Lantern.Query.withNoArguments model.query

                ( lanternState, cmd ) =
                    Lantern.query model.lanternState query (editorQueryResultDecoder |> Json.Decode.list |> Json.Decode.map EditorQuery) QueryResult
            in
            ( { model | lanternState = lanternState }, cmd )

        RunDdl ->
            let
                ddlQuery =
                    Lantern.Query.withNoArguments model.ddl

                ( lanternState, cmd ) =
                    Lantern.migrate ddlQuery DdlResult model.lanternState
            in
            ( { model | ddlResult = False, lanternState = lanternState }, cmd )

        DdlResult r ->
            ( { model | ddlResult = r }, Cmd.none )

        QueryResult result ->
            case result of
                Err error ->
                    ( { model | queryError = Just error }, Cmd.none )

                Ok (EditorQuery queryResult) ->
                    ( { model | queryResult = queryResult }, Cmd.none )

                Ok (TablesQuery queryResult) ->
                    ( { model | tables = queryResult }, Cmd.none )

        LanternMessage message ->
            let
                ( lanternState, lanternCmd ) =
                    Lantern.update message model.lanternState
            in
            ( { model | lanternState = lanternState }, lanternCmd )



-- VIEW


logView : Lantern.Log.Log -> Html Msg
logView log =
    Html.ul []
        (log.lines
            |> List.map (\( _, line ) -> Html.li [] [ text line ])
        )


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ Html.Events.onSubmit RunQuery ]
            [ div [] [ Html.textarea [ onInput UpdateQuery, Html.Attributes.cols 80 ] [] ]
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run query" ] [] ]
            , div [] [ text ("Results: " ++ Debug.toString model.queryResult) ]
            , div [] [ text ("Server error: " ++ (model.queryError |> Maybe.map Lantern.errorToString |> Maybe.withDefault "")) ]
            ]
        , Html.form [ Html.Events.onSubmit RunDdl ]
            [ div [] [ Html.textarea [ onInput UpdateDdl, Html.Attributes.cols 80 ] [] ]
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run DDL" ] [] ]
            , div [] [ text ("Result: " ++ Debug.toString model.ddlResult) ]
            , div [] [ text "Tables:", Html.ul [] (List.map (\{ name } -> Html.li [] [ text name ]) model.tables) ]
            ]
        , Html.form [ Html.Events.onSubmit RunPing ]
            [ div [] [ Html.textarea [ onInput UpdatePing, Html.Attributes.cols 80 ] [] ]
            , div [] [ input [ Html.Attributes.type_ "submit", Html.Attributes.value "Run echo" ] [] ]
            , div [] [ text ("Results: " ++ Debug.toString model.pong) ]
            ]
        , logView model.lanternState.log
        ]
