module DevTools.Apps.Migrations exposing (App, Message, lanternApp)

import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.Query
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { query : String
    , result : Maybe Bool
    }


type alias App =
    Lantern.App Context Model Message


lanternApp : App
lanternApp =
    Lantern.simpleApp
        { model = init
        , view = view
        , update = update
        }


type Message
    = Update String
    | HandleResult Bool
    | Run


init : Model
init =
    { query = ""
    , result = Nothing
    }


update : Context -> Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
update _ msg model =
    case msg of
        Update query ->
            ( { model | query = query }, Cmd.none )

        Run ->
            let
                migration =
                    Lantern.Query.withNoArguments model.query
            in
            ( model, Lantern.migrate migration HandleResult )

        HandleResult result ->
            ( { model | result = Just result }, Cmd.none )


view : Context -> Model -> Element (Lantern.Message Message)
view { theme } { query } =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = Update >> Lantern.AppMessage
            , text = query
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Migration"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.AppMessage Run)
            , label = Element.text "Run migration"
            }
        ]
