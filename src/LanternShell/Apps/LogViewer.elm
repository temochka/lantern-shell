module LanternShell.Apps.LogViewer exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Font
import Lantern
import Lantern.App
import Lantern.Log exposing (Log)
import LanternUi
import LanternUi.Theme


type alias Context =
    { log : Log, theme : LanternUi.Theme.Theme }


type alias Model =
    ()


type alias Message =
    ()


init : Model
init =
    ()


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update _ model =
    ( model, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { log, theme } _ =
    LanternUi.columnLayout
        theme
        []
        [ log
            |> .lines
            |> List.map (\( _, line ) -> Element.text line)
            |> Element.column
                [ Element.width Element.fill
                , LanternUi.listSpacing
                , Element.Font.family [ Element.Font.typeface "Monaco", Element.Font.typeface "Fira Mono", Element.Font.monospace ]
                ]
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { init = init
        , view = view
        , update = update
        }