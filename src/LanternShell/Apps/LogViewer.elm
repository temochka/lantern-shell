module LanternShell.Apps.LogViewer exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Background
import Element.Font
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
            |> List.map (\( _, line ) -> Element.paragraph [ Element.width Element.fill ] [ Element.text line ])
            |> Element.textColumn
                [ Element.padding 5
                , Element.width Element.fill
                , Element.scrollbarX
                , Element.Background.color theme.bgContrast
                , Element.Font.color theme.fontContrast
                , Element.Font.size 12
                , Element.spacing 10
                , Element.Font.family [ Element.Font.typeface "Monaco", Element.Font.typeface "Fira Mono", Element.Font.monospace ]
                ]
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Logs"
        , init = init
        , view = view
        , update = update
        }
