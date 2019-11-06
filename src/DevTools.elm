port module DevTools exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Lantern
import Lantern.Query
import Lantern.Request
import String


port lanternRequestPort : Lantern.RequestPort msg


port lanternResponsePort : Lantern.ResponsePort msg



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias QueryResult =
    ( Int, Int )


decodeQueryResults : List Json.Decode.Value -> Msg
decodeQueryResults results =
    let
        decoder =
            Json.Decode.map2
                Tuple.pair
                (Json.Decode.field "1" Json.Decode.int)
                (Json.Decode.field "2" Json.Decode.int)
    in
    results
        |> List.map (Json.Decode.decodeValue decoder >> Result.toMaybe)
        |> List.filterMap identity
        |> QueryResult


type alias Model =
    { query : String
    , queryResult : List QueryResult
    , ping : String
    , pong : String
    , serverResponse : Maybe String
    , lanternState : Lantern.State Msg
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( lanternState, lanternCmd ) =
            Lantern.init lanternRequestPort lanternResponsePort LanternMessage
    in
    ( { query = ""
      , queryResult = []
      , ping = ""
      , pong = ""
      , serverResponse = Nothing
      , lanternState = lanternState
      }
    , lanternCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Lantern.subscriptions model.lanternState



-- UPDATE


type Msg
    = UpdateQuery String
    | UpdatePing String
    | RunQuery
    | RunPing
    | ReceivePong String
    | QueryResult (List QueryResult)
    | LanternMessage Lantern.Message


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        UpdatePing ping ->
            ( { model | ping = ping }, Cmd.none )

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
                    Lantern.query model.lanternState query decodeQueryResults
            in
            ( { model | lanternState = lanternState }, cmd )

        QueryResult results ->
            ( { model | queryResult = results }, Cmd.none )

        LanternMessage message ->
            let
                ( lanternState, lanternCmd ) =
                    Lantern.update message model.lanternState
            in
            ( { model | lanternState = lanternState }, lanternCmd )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ Html.Attributes.type_ "text", onInput UpdateQuery ] []
            , button [ onClick RunQuery ] [ text "Run query" ]
            , div [] [ text ("Results: " ++ Debug.toString model.queryResult) ]
            , div [] [ text ("Server responses: " ++ Debug.toString model.lanternState.selectQueryResponses) ]
            ]
        , div []
            [ input [ Html.Attributes.type_ "text", onInput UpdatePing ] []
            , button [ onClick RunPing ] [ text "Run echo" ]
            , div [] [ text ("Results: " ++ Debug.toString model.pong) ]
            , div [] [ text ("Server responses: " ++ Debug.toString model.lanternState.echoResponses) ]
            ]
        ]
