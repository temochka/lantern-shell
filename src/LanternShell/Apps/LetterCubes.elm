module LanternShell.Apps.LetterCubes exposing (..)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Input
import Lantern.App
import LanternUi
import LanternUi.Input
import LanternUi.Theme
import Set exposing (Set)


type alias Context =
    { theme : LanternUi.Theme.Theme }


type alias Model =
    { text : String
    , validPath : Maybe (List Int)
    , saved : List String
    , remainingLetters : List Char
    }


type Cube
    = Cube (List Char)


cubes : List Cube
cubes =
    [ Cube [ 'п', 'ъ', 'н', 'д', 'щ', 'л' ]
    , Cube [ 'м', 'о', 'ж', 'е', 'г', 'х', 'ш' ]
    , Cube [ 'п', 'э', 'ю', 'ы', 'й', 'к' ]
    , Cube [ 'у', 'з', 'я', 'а', 'и', 'в' ]
    , Cube [ 'ё', 'ш', 'м', 'с', 'т', 'б', 'е' ]
    , Cube [ 'а', 'ф', 'ц', 'ч', 'р', 'ь' ]
    ]


cubesIndex : Dict Char (List Int)
cubesIndex =
    cubes
        |> List.indexedMap Tuple.pair
        |> List.concatMap (\( i, Cube l ) -> List.map (\ch -> ( ch, i )) l)
        |> List.foldr (\( ch, i ) a -> Dict.update ch (Maybe.withDefault [] >> (::) i >> Just) a) Dict.empty


validate : List Char -> Dict Char (List Int) -> Maybe (List Int)
validate chars index =
    case chars of
        [] ->
            Just []

        ' ' :: rest ->
            validate rest index

        ch :: rest ->
            Dict.get ch index
                |> Maybe.andThen
                    (List.foldl
                        (\cubeIndex a ->
                            if a == Nothing then
                                validate rest (Dict.map (\_ v -> List.filter ((/=) cubeIndex) v) index)
                                    |> Maybe.map ((::) cubeIndex)

                            else
                                a
                        )
                        Nothing
                    )


type Message
    = UpdateText String
    | SaveWord


init : Model
init =
    { text = ""
    , validPath = Just []
    , saved = []
    , remainingLetters = cubesIndex |> Dict.keys
    }


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        UpdateText text ->
            let
                validPath =
                    validate (String.toList text) cubesIndex

                usedCubes =
                    validPath |> Maybe.map Set.fromList |> Maybe.withDefault Set.empty

                remainingLetters =
                    cubesIndex
                        |> Dict.filter (\_ indexes -> not <| List.all (\i -> Set.member i usedCubes) indexes)
                        |> Dict.keys
            in
            ( { model | text = text, validPath = validPath, remainingLetters = remainingLetters }, Cmd.none )

        SaveWord ->
            if model.validPath /= Nothing then
                ( { model | saved = model.text :: model.saved }, Cmd.none )

            else
                ( model, Cmd.none )


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } model =
    let
        validStatus =
            if model.validPath /= Nothing then
                "Valid!"

            else
                "Invalid"
    in
    Element.row
        [ Element.width Element.fill ]
        [ LanternUi.columnLayout
            theme
            []
            [ model.remainingLetters
                |> List.map (String.fromChar >> Element.text)
                |> List.intersperse (Element.text ", ")
                |> Element.wrappedRow []
            , LanternUi.Input.text theme
                []
                { onChange = UpdateText >> Lantern.App.Message
                , text = model.text
                , placeholder = Nothing
                , label = Element.Input.labelHidden "Word"
                }
            , Element.text validStatus
            , LanternUi.Input.button theme [] { label = Element.text "Save", onPress = Just (SaveWord |> Lantern.App.Message) }
            ]
        , LanternUi.panel
            theme
            []
            { content = model.saved |> List.map Element.text |> Element.column [ Element.width Element.fill ]
            , header = Just <| LanternUi.textPanelHeader [] "Saved words"
            }
        ]


lanternApp : Lantern.App.App Context () Model Message
lanternApp =
    Lantern.App.simpleApp
        { name = "Letter cubes"
        , init = init
        , view = view
        , update = update
        }
