module DevTools.Apps.LogViewer exposing (App, Message, lanternApp)

import Element exposing (Element)
import Element.Font
import Lantern
import Lantern.Log exposing (Log)
import LanternUi
import LanternUi.Theme


type alias Context =
    { log : Log, theme : LanternUi.Theme.Theme }


type alias Model =
    ()


type alias Message =
    ()


type alias App =
    Lantern.App Context Model Message


lanternApp : App
lanternApp =
    Lantern.simpleApp
        { model = init
        , update = update
        , view = view
        }


init : Model
init =
    ()


update : Context -> Message -> Model -> ( Model, Cmd (Lantern.Message Message) )
update _ _ model =
    ( model, Cmd.none )


view : Context -> Model -> Element (Lantern.Message Message)
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
