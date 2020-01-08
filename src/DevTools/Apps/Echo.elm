module DevTools.Apps.Echo exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { ping : String
    , pong : String
    }


type Message
    = UpdatePing String
    | ReceivePong String
    | Run


init : Model
init =
    { ping = ""
    , pong = ""
    }


update : Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
update msg model =
    case msg of
        UpdatePing ping ->
            ( { model | ping = ping }, Cmd.none )

        Run ->
            ( model, Lantern.echo model.ping ReceivePong )

        ReceivePong pong ->
            ( { model | pong = pong }, Cmd.none )


view : Context -> Model -> Element (Lantern.Message Message)
view { theme } model =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = UpdatePing >> Lantern.AppMessage
            , text = model.ping
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Echo"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.AppMessage Run)
            , label = Element.text "Run echo"
            }
        , Element.text ("Results: " ++ Debug.toString model.pong)
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { init = init
        , view = view
        , update = update
        }
