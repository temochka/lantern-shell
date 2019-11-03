port module DevTools exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Json.Decode
import Json.Encode
import Lantern
import String


port lanternRequest : Lantern.RequestPort msg


port lanternResponse : Lantern.ResponsePort msg



-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { query : String
    , serverResponse : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { query = "", serverResponse = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    lanternResponse LanternResponse



-- UPDATE


type Msg
    = UpdateQuery String
    | RunQuery
    | LanternResponse Lantern.Response


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        RunQuery ->
            ( model, lanternRequest ("{\"id\":\"42\",\"type\":\"Query\",\"query\":\"" ++ model.query ++ "\"}") )

        LanternResponse response ->
            ( { model | serverResponse = Just response }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ Html.Attributes.type_ "text", onInput UpdateQuery ] []
        , button [ onClick RunQuery ] [ text "Run" ]
        , div [] [ text ("Server response: " ++ (model.serverResponse |> Maybe.withDefault "none")) ]
        ]
