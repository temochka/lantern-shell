module LanternShell.Apps.AppLauncher exposing (Flags, Launcher(..), Message(..), Model, init, lanternApp)

import Element exposing (Element)
import Element.Font
import Element.Input
import Lantern
import Lantern.App
import Lantern.LiveQuery exposing (LiveQuery)
import LanternShell.Apps.Scripts exposing (Script, ScriptId)
import LanternUi.FuzzySelect
import LanternUi.Theme
import Task


type alias Flags launcher =
    { nativeApps : List ( String, Launcher launcher ) }


type alias Context =
    { theme : LanternUi.Theme.Theme }


type Launcher launcher
    = UserScript ( ScriptId, Script )
    | NativeApp launcher


type alias Model launcher =
    { appLauncherQuery : String
    , fuzzySelect : LanternUi.FuzzySelect.FuzzySelect
    , nativeApps : List ( String, Launcher launcher )
    , options : List ( String, Launcher launcher )
    }


type Message launcher
    = FuzzySelectMessage LanternUi.FuzzySelect.Message
    | PrelaunchApp (Launcher launcher)
    | LaunchApp (Launcher launcher)
    | UpdateLauncherQuery String
    | UpdateScripts (Result Lantern.Error (List ( ScriptId, Script )))


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

        PrelaunchApp launcher ->
            ( { model | appLauncherQuery = "" }
            , Task.succeed launcher |> Task.perform (LaunchApp >> Lantern.App.Message)
            )

        -- This never runs in practice, handled in LanternShell.Apps
        LaunchApp _ ->
            ( model, Cmd.none )

        UpdateScripts result ->
            case result of
                Ok scripts ->
                    ( { model
                        | options =
                            scripts
                                |> List.map (\( scriptId, s ) -> ( "Scripts 〉" ++ s.name, UserScript ( scriptId, s ) ))
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
        , Element.paddingXY 5 10
        ]
        [ Element.el [ Element.Font.color theme.fontContrastInactive ] (Element.text ">")
        , LanternUi.FuzzySelect.fuzzySelect
            theme
            { options = model.options
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Launch app"
            , id = Just "lanternAppLauncher"
            , onInternalMessage = FuzzySelectMessage >> Lantern.App.Message
            , onOptionSelect = PrelaunchApp >> Lantern.App.Message
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
        , titleBarAddOns = \_ _ -> Element.none
        , update = update
        , subscriptions = always Sub.none
        , decodeFlags = always Nothing
        , encodeFlags = always Nothing
        }
