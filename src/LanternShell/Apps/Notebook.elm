module LanternShell.Apps.Notebook exposing (Message, Model, init, lanternApp)

import Element exposing (Element)
import Element.Input
import Enclojure
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { code : String
    , result : String
    }


type Message
    = SetCode String
    | Eval


init : Model
init =
    { code = ""
    , result = ""
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        SetCode code ->
            ( { model | code = code }, Cmd.none )

        Eval ->
            let
                result =
                    Enclojure.eval model.code
            in
            ( { model | result = Debug.toString result }, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } model =
    LanternUi.columnLayout
        theme
        []
        [ LanternUi.Input.multiline theme
            []
            { onChange = SetCode >> Lantern.App.Message
            , text = model.code
            , placeholder = Nothing
            , spellcheck = False
            , label = Element.Input.labelHidden "Code"
            }
        , LanternUi.Input.button theme
            []
            { onPress = Just (Lantern.App.Message Eval)
            , label = Element.text "Eval"
            }
        , Element.text ("Result: " ++ model.result)
        ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Notebook"
        , init = init
        , view = view
        , update = update
        }
