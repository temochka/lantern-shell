module LanternShell.Apps.ValueInspector exposing (Flags, Message, Model, init, lanternApp)

import Element exposing (Element)
import Enclojure.Runtime
import Enclojure.Types exposing (Value(..))
import Lantern.App
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Flags =
    { value : Value }


type alias Model =
    { value : Value
    }


type Message
    = Nop


init : Maybe Flags -> ( Model, Cmd (Lantern.App.Message Message) )
init flags =
    ( { value = flags |> Maybe.map .value |> Maybe.withDefault Nil
      }
    , Cmd.none
    )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view ctx model =
    Element.text (Enclojure.Runtime.inspect model.value)


lanternApp : Lantern.App.App Context Flags Model Message
lanternApp =
    Lantern.App.app
        { name = "Value inspector"
        , init = init
        , liveQueries = Nothing
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
