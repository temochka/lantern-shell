port module DevTools exposing (..)

import Browser
import Debug
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
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
    { clientCount : Int
    , serverCount : Int
    , serverResponse : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { clientCount = 0, serverCount = 0, serverResponse = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : model -> Sub Msg
subscriptions _ =
    lanternResponse LanternResponse



-- UPDATE


type Msg
    = Increment
    | Decrement
    | LanternResponse Lantern.Response


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.clientCount + 1
            in
            ( { model | clientCount = newCount }, lanternRequest ("{\"id\":\"42\",\"type\":\"Echo\",\"text\":\"" ++ String.fromInt newCount ++ "\"}") )

        Decrement ->
            let
                newCount =
                    model.clientCount - 1
            in
            ( { model | clientCount = newCount }, lanternRequest (String.fromInt newCount) )

        LanternResponse response ->
            case String.toInt response of
                Just newServerCount ->
                    ( { model | serverCount = newServerCount, serverResponse = Just response }, Cmd.none )

                Nothing ->
                    ( { model | serverResponse = Just response }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.clientCount) ]
        , button [ onClick Increment ] [ text "+" ]
        , div [] [ text ("Server: " ++ String.fromInt model.serverCount) ]
        , div [] [ text ("Server response: " ++ (model.serverResponse |> Maybe.withDefault "none")) ]
        ]
