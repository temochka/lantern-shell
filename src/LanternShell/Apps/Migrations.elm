module LanternShell.Apps.Migrations exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Input
import Lantern
import Lantern.App
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


type Message
    = Update String
    | HandleResult Bool
    | Run


init : Model
init =
    { query = ""
    , result = Nothing
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        Update query ->
            ( { model | query = query }, Cmd.none )

        Run ->
            let
                migration =
                    Lantern.Query.withNoArguments model.query
            in
            ( model, Lantern.migrate migration HandleResult |> Lantern.App.call )

        HandleResult result ->
            ( { model | result = Just result }, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } { query } =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = Update >> Lantern.App.Message
            , text = query
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Migration"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.App.Message Run)
            , label = Element.text "Run migration"
            }
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Migrations"
        , init = init
        , view = view
        , update = update
        }
