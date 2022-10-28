module LanternShell.Apps.AppLauncher exposing (Launcher(..), Message(..), Model, init, lanternApp)

import Element exposing (Element)
import Element.Font
import Element.Input
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery)
import LanternShell.Apps.Scripts exposing (Script)
import LanternUi.FuzzySelect
import LanternUi.Theme


type alias Flags launcher =
    { nativeApps : List ( String, Launcher launcher ) }


type alias Context =
    { theme : LanternUi.Theme.Theme }


type Launcher launcher
    = UserScript Script
    | NativeApp launcher


type alias Model launcher =
    { appLauncherQuery : String
    , fuzzySelect : LanternUi.FuzzySelect.FuzzySelect
    , nativeApps : List ( String, Launcher launcher )
    , options : List ( String, Launcher launcher )
    }


type Message launcher
    = FuzzySelectMessage LanternUi.FuzzySelect.Message
    | LaunchApp (Launcher launcher)
    | UpdateLauncherQuery String
    | UpdateScripts (Result Lantern.Error (List Script))


init : Maybe (Flags launcher) -> ( Model launcher, Cmd (Lantern.App.Message (Message launcher)) )
init flags =
    let
        nativeApps =
            flags |> Maybe.map .nativeApps |> Maybe.withDefault []
    in
    ( { appLauncherQuery = ""
      , fuzzySelect = LanternUi.FuzzySelect.hidden
      , nativeApps = nativeApps
      , options = nativeApps
      }
    , Cmd.none
    )


update : Message launcher -> Model launcher -> ( Model launcher, Cmd (Lantern.App.Message (Message launcher)) )
update msg model =
    case msg of
        UpdateLauncherQuery query ->
            ( { model | appLauncherQuery = query }, Cmd.none )

        FuzzySelectMessage proxiedMsg ->
            ( { model | fuzzySelect = LanternUi.FuzzySelect.update proxiedMsg model.fuzzySelect }, Cmd.none )

        LaunchApp _ ->
            ( model, Cmd.none )

        UpdateScripts result ->
            case result of
                Ok scripts ->
                    ( { model
                        | options =
                            scripts
                                |> List.map (\s -> ( "Scripts 〉" ++ s.name, UserScript s ))
                                |> (++) model.nativeApps
                                |> List.sortBy (Tuple.first >> String.toLower)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


view : Context -> Model launcher -> Element (Lantern.App.Message (Message launcher))
view { theme } model =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 10
        , Element.paddingXY 25 10
        ]
        [ Element.el [ Element.Font.color theme.fontContrastInactive ] (Element.text ">")
        , LanternUi.FuzzySelect.fuzzySelect
            theme
            { options = model.options
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Launch app"
            , id = Just "lanternAppLauncher"
            , onInternalMessage = FuzzySelectMessage >> Lantern.App.Message
            , onOptionSelect = LaunchApp >> Lantern.App.Message
            , onQueryChange = UpdateLauncherQuery >> Lantern.App.Message
            , state = model.fuzzySelect
            , query = model.appLauncherQuery
            }
        ]


liveQueries : Model launcher -> List (LiveQuery (Message launcher))
liveQueries _ =
    [ LanternShell.Apps.Scripts.scriptsQuery UpdateScripts ]


lanternApp : Lantern.App.App Context (Flags launcher) (Model launcher) (Message launcher)
lanternApp =
    Lantern.App.app
        { name = "Launch app"
        , liveQueries = Just liveQueries
        , init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
