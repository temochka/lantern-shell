module LanternShell.Apps.FlashcardGenerator exposing (Message, Model, init, lanternApp, update, view)

import Csv.Encode
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Font
import Element.Input
import File.Download
import Html.String
import Json.Decode
import Lantern
import Lantern.App
import Lantern.Http
import Lantern.Query
import LanternUi.FuzzySelect exposing (FuzzySelect)
import LanternUi.Input
import LanternUi.Theme
import Url.Builder


appIdDbKey : String
appIdDbKey =
    "OXFORD_DICTIONARIES_APP_ID"


apiKeyDbKey : String
apiKeyDbKey =
    "OXFORD_DICTIONARIES_API_KEY"


type alias Context =
    { theme : LanternUi.Theme.Theme }


type Message
    = UpdateApiCredentials ApiCredentials
    | FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | ConfigUpdated (Result Lantern.Error Lantern.Query.WriterResult)
    | UpdateUserInput String
    | LoadDefinition String (Result Lantern.Http.Error Definition)
    | LoadTranslation String (Result Lantern.Http.Error Translation)
    | UpdateUserTranslation String String
    | LoadedCredentials (Result Lantern.Error (List ( String, String )))
    | NextStep
    | DownloadCards


type UiState
    = Config { apiCredentials : Maybe ApiCredentials }
    | Input { apiCredentials : ApiCredentials }
    | Cards { apiCredentials : ApiCredentials }
    | Export { apiCredentials : ApiCredentials }


type Sense
    = Sense
        { definitions : List String
        , examples : List String
        , subsenses : List Sense
        , id : String
        }


type alias LexicalEntry =
    { pos : String
    , pronunciation : Maybe String
    , senses : List Sense
    }


type alias Definition =
    { word : String
    , lexicalEntries : List LexicalEntry
    }


type alias TranslationEntry =
    { pos : String
    , translations : List String
    }


type alias Translation =
    { word : String
    , translationEntries : List TranslationEntry
    }


type Remote a
    = Loading
    | Errored String
    | Loaded a


type alias Word =
    { id : String
    , definition : Remote Definition
    , translation : Remote Translation
    }


type alias ApiCredentials =
    { appId : String
    , appKey : String
    }


type alias UserTranslations =
    Dict String String


type alias Model =
    { userInput : String
    , words : List String
    , cache : Dict String Word
    , userTranslations : UserTranslations
    , uiState : UiState
    , fuzzySelect : ( String, FuzzySelect )
    }


type alias FlashCard =
    { front : Element (Lantern.App.Message Message)
    , back : Element (Lantern.App.Message Message)
    }


type alias FlashCardForExport =
    { front : Html.String.Html (Lantern.App.Message Message)
    , back : Html.String.Html (Lantern.App.Message Message)
    }


credentialsDecoder : Json.Decode.Decoder ( String, String )
credentialsDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "value" Json.Decode.string)


listWithDefault : a -> List a -> List a
listWithDefault default list =
    if List.isEmpty list then
        [ default ]

    else
        list


obscureWord : String -> String
obscureWord word =
    String.left 1 word
        ++ "…"
        ++ (if String.length word > 3 then
                String.right 1 word

            else
                ""
           )


obscureDefinition : String -> String -> String
obscureDefinition word =
    String.replace word "[…]"


renderFlashCardForExport : UserTranslations -> Definition -> FlashCardForExport
renderFlashCardForExport userTranslations { word, lexicalEntries } =
    let
        formatSenseFront (Sense { examples }) =
            examples
                |> listWithDefault "-"
                |> List.map (\example -> Html.String.i [] [ Html.String.text example ])
                |> List.intersperse (Html.String.text " | ")
                |> Html.String.li []

        formatSenseBack (Sense { id, definitions, subsenses }) =
            Html.String.li []
                [ Html.String.text (definitions |> String.join ";" |> obscureDefinition word)
                , case Dict.get id userTranslations |> Maybe.withDefault "" of
                    "" ->
                        Html.String.text ""

                    translation ->
                        Html.String.text (" (" ++ translation ++ ")")
                , if List.isEmpty subsenses then
                    Html.String.text ""

                  else
                    subsenses |> List.map formatSenseBack |> Html.String.ul []
                ]

        front =
            lexicalEntries
                |> List.map
                    (\{ pronunciation, pos, senses } ->
                        Html.String.p []
                            [ Html.String.text word
                            , Html.String.text " "
                            , Html.String.text ("[" ++ (pronunciation |> Maybe.withDefault "?") ++ "]")
                            , Html.String.text " "
                            , Html.String.text ("(" ++ pos ++ "),\n")
                            , senses |> List.map formatSenseFront |> Html.String.ol []
                            ]
                    )
                |> Html.String.div []

        back =
            lexicalEntries
                |> List.map
                    (\{ senses } ->
                        Html.String.div []
                            [ Html.String.p [] [ Html.String.text ("[" ++ obscureWord word ++ "]") ]
                            , senses |> List.map formatSenseBack |> Html.String.ol []
                            ]
                    )
                |> Html.String.div []
    in
    { front = front
    , back = back
    }


renderFlashCard :
    LanternUi.Theme.Theme
    ->
        { fuzzySelectState : ( String, FuzzySelect )
        , userTranslations : UserTranslations
        , definition : Definition
        , translation : Translation
        }
    -> FlashCard
renderFlashCard theme ({ userTranslations, definition, translation } as options) =
    let
        { word, lexicalEntries } =
            definition

        ( fuzzySelectSenseId, fuzzySelectState ) =
            options.fuzzySelectState

        formatSenseFront (Sense { examples }) =
            examples
                |> listWithDefault "–"
                |> List.map (\example -> Element.el [ Element.Font.italic ] (Element.text example))
                |> List.intersperse (Element.text " | ")
                |> Element.paragraph [ Element.width Element.fill ]

        formatSenseBack marker entryPos (Sense { id, definitions, subsenses }) =
            Element.row [ Element.width Element.fill, Element.spacing 5 ]
                [ Element.el [ Element.alignTop ] (Element.text marker)
                , Element.paragraph [ Element.width Element.fill ]
                    [ Element.text (definitions |> String.join ";" |> obscureDefinition word)
                    , LanternUi.FuzzySelect.fuzzySelect
                        theme
                        { onQueryChange = UpdateUserTranslation id >> Lantern.App.Message
                        , query = Dict.get id userTranslations |> Maybe.withDefault ""
                        , placeholder = Just (Element.Input.placeholder [] (Element.text "Translation"))
                        , label = Element.Input.labelHidden "Translation"
                        , id = Nothing
                        , onInternalMessage = FuzzySelectMessage id >> Lantern.App.Message
                        , onOptionSelect = UpdateUserTranslation id >> Lantern.App.Message
                        , options =
                            translation.translationEntries
                                |> List.filterMap
                                    (\{ pos, translations } ->
                                        if pos == entryPos then
                                            Just translations

                                        else
                                            Nothing
                                    )
                                |> List.concat
                                |> List.map (\t -> ( t, t ))
                        , state =
                            if id == fuzzySelectSenseId then
                                fuzzySelectState

                            else
                                Nothing
                        }
                    , if List.isEmpty subsenses then
                        Element.none

                      else
                        subsenses |> List.map (formatSenseBack "◦" entryPos) |> Element.column [ Element.width Element.fill, Element.spacing 5 ]
                    ]
                ]

        front =
            lexicalEntries
                |> List.map
                    (\{ pronunciation, pos, senses } ->
                        Element.column
                            [ Element.width Element.fill, Element.spacing 5 ]
                            [ Element.paragraph []
                                [ Element.text word
                                , Element.text " "
                                , Element.text ("[" ++ (pronunciation |> Maybe.withDefault "?") ++ "]")
                                , Element.text " "
                                , Element.text ("(" ++ pos ++ "),\n")
                                ]
                            , senses
                                |> List.indexedMap
                                    (\i el ->
                                        Element.row [ Element.width Element.fill, Element.spacing 5 ]
                                            [ Element.el [] (Element.text (String.fromInt (i + 1) ++ "."))
                                            , formatSenseFront el
                                            ]
                                    )
                                |> Element.column [ Element.width Element.fill, Element.spacing 5 ]
                            ]
                    )
                |> Element.column [ Element.width Element.fill ]

        back =
            lexicalEntries
                |> List.map
                    (\{ pos, senses } ->
                        Element.column [ Element.width Element.fill, Element.spacing 5 ]
                            [ Element.paragraph [] [ Element.text ("[" ++ obscureWord word ++ "]") ]
                            , senses
                                |> List.indexedMap
                                    (\i el ->
                                        formatSenseBack (String.fromInt (i + 1) ++ ".") pos el
                                    )
                                |> Element.column [ Element.width Element.fill, Element.spacing 5 ]
                            ]
                    )
                |> Element.column [ Element.width Element.fill, Element.spacing 5 ]
    in
    { front = front
    , back = back
    }


sense : Json.Decode.Decoder Sense
sense =
    Json.Decode.map4
        (\id definitions examples subsenses -> Sense { definitions = definitions, examples = examples, subsenses = subsenses, id = id })
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.oneOf [ Json.Decode.field "definitions" (Json.Decode.list Json.Decode.string), Json.Decode.succeed [] ])
        (Json.Decode.oneOf [ Json.Decode.field "examples" (Json.Decode.list (Json.Decode.field "text" Json.Decode.string)), Json.Decode.succeed [] ])
        (Json.Decode.oneOf [ Json.Decode.field "subsenses" (Json.Decode.list (Json.Decode.lazy (\_ -> sense))), Json.Decode.succeed [] ])


definitionDecoder : Json.Decode.Decoder Definition
definitionDecoder =
    let
        findIpa =
            Json.Decode.oneOf
                [ Json.Decode.list
                    (Json.Decode.map2 Tuple.pair
                        (Json.Decode.field "phoneticNotation" Json.Decode.string)
                        (Json.Decode.field "phoneticSpelling" Json.Decode.string)
                    )
                , Json.Decode.succeed []
                ]
                |> Json.Decode.map
                    (List.filter (\( notation, _ ) -> notation == "IPA")
                        >> List.head
                        >> Maybe.map Tuple.second
                    )

        lexicalEntry =
            Json.Decode.map3 LexicalEntry
                (Json.Decode.at [ "lexicalCategory", "id" ] Json.Decode.string)
                (Json.Decode.field "pronunciations" findIpa)
                (Json.Decode.field "entries" (Json.Decode.index 0 (Json.Decode.field "senses" (Json.Decode.list sense))))

        result =
            Json.Decode.map2 Definition
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "lexicalEntries" (Json.Decode.list lexicalEntry))
    in
    Json.Decode.field "results" (Json.Decode.index 0 result)


translationDecoder : Json.Decode.Decoder Translation
translationDecoder =
    let
        translationSense =
            Json.Decode.oneOf [ Json.Decode.field "translations" (Json.Decode.list (Json.Decode.field "text" Json.Decode.string)), Json.Decode.succeed [] ]

        translationEntry =
            Json.Decode.map2 TranslationEntry
                (Json.Decode.at [ "lexicalCategory", "id" ] Json.Decode.string)
                (Json.Decode.field "entries" (Json.Decode.index 0 (Json.Decode.field "senses" (Json.Decode.list translationSense |> Json.Decode.map List.concat))))

        result =
            Json.Decode.map2 Translation
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "lexicalEntries" (Json.Decode.list translationEntry))
    in
    Json.Decode.field "results" (Json.Decode.index 0 result)


init : ( Model, Cmd (Lantern.App.Message Message) )
init =
    ( { userInput = ""
      , words = []
      , cache = Dict.empty
      , uiState = Config { apiCredentials = Nothing }
      , userTranslations = Dict.empty
      , fuzzySelect = ( "", Nothing )
      }
    , Lantern.readerQuery
        (Lantern.Query.withArguments
            "SELECT name, value FROM apiCredentials WHERE name IN ($appIdDbKey, $apiKeyDbKey)"
            [ ( "$appIdDbKey", Lantern.Query.String appIdDbKey )
            , ( "$apiKeyDbKey", Lantern.Query.String apiKeyDbKey )
            ]
        )
        credentialsDecoder
        LoadedCredentials
        |> Lantern.App.call
    )


parseWords : String -> List String
parseWords =
    String.lines


fetchDefinition : ApiCredentials -> String -> Cmd (Lantern.App.Message Message)
fetchDefinition { appId, appKey } word =
    Lantern.httpRequest
        { method = "GET"
        , headers =
            [ ( "app_id", appId )
            , ( "app_key", appKey )
            ]
        , body = Nothing
        , url =
            Url.Builder.crossOrigin "https://od-api.oxforddictionaries.com"
                [ "api", "v2", "entries", "en-us", word ]
                []
        , expect = Lantern.Http.expectJson (LoadDefinition word) definitionDecoder
        }
        |> Lantern.App.call


fetchTranslation : ApiCredentials -> String -> Cmd (Lantern.App.Message Message)
fetchTranslation { appId, appKey } word =
    Lantern.httpRequest
        { method = "GET"
        , headers =
            [ ( "app_id", appId )
            , ( "app_key", appKey )
            ]
        , body = Nothing
        , url =
            Url.Builder.crossOrigin "https://od-api.oxforddictionaries.com"
                [ "api", "v2", "translations", "en", "ru", word ]
                []
        , expect = Lantern.Http.expectJson (LoadTranslation word) translationDecoder
        }
        |> Lantern.App.call


refreshCache : ApiCredentials -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
refreshCache apiCredentials ({ words, cache } as model) =
    let
        cacheDefault default current =
            case current of
                Nothing ->
                    Just default

                _ ->
                    current

        newCache =
            List.foldl
                (\word currentCache -> Dict.update word (cacheDefault { id = word, definition = Loading, translation = Loading }) currentCache)
                cache
                words

        definitionRequests =
            newCache
                |> Dict.values
                |> List.filterMap
                    (\{ id, definition } ->
                        if definition == Loading then
                            Just (fetchDefinition apiCredentials id)

                        else
                            Nothing
                    )

        translationRequests =
            newCache
                |> Dict.values
                |> List.filterMap
                    (\{ id, translation } ->
                        if translation == Loading then
                            Just (fetchTranslation apiCredentials id)

                        else
                            Nothing
                    )
    in
    ( { model | cache = newCache }, definitionRequests ++ translationRequests |> Cmd.batch )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg model =
    case msg of
        LoadDefinition word result ->
            let
                definition =
                    case result of
                        Ok def ->
                            Loaded def

                        Err err ->
                            Errored (Lantern.Http.errorToString err)
            in
            ( { model
                | cache =
                    Dict.update word
                        (\current ->
                            case current of
                                Just w ->
                                    Just { w | definition = definition }

                                Nothing ->
                                    current
                        )
                        model.cache
              }
            , Cmd.none
            )

        LoadTranslation word result ->
            let
                translation =
                    case result of
                        Ok def ->
                            Loaded def

                        Err err ->
                            Errored (Lantern.Http.errorToString err)
            in
            ( { model
                | cache =
                    Dict.update word
                        (\current ->
                            case current of
                                Just w ->
                                    Just { w | translation = translation }

                                Nothing ->
                                    current
                        )
                        model.cache
              }
            , Cmd.none
            )

        UpdateUserTranslation senseId translation ->
            ( { model | userTranslations = Dict.insert senseId translation model.userTranslations }, Cmd.none )

        UpdateUserInput userInput ->
            ( { model | userInput = userInput }, Cmd.none )

        NextStep ->
            case model.uiState of
                Config { apiCredentials } ->
                    case apiCredentials of
                        Just ({ appId, appKey } as creds) ->
                            ( { model | uiState = Input { apiCredentials = creds } }
                            , Lantern.writerQuery
                                (Lantern.Query.withArguments
                                    """
                                        INSERT INTO
                                            apiCredentials (name, value)
                                        VALUES
                                            ($appIdDbKey, $appId),
                                            ($apiKeyDbKey, $apiKey)
                                        ON CONFLICT(name) DO UPDATE SET value=excluded.value
                                    """
                                    [ ( "$appIdDbKey", Lantern.Query.String appIdDbKey )
                                    , ( "$appId", Lantern.Query.String appId )
                                    , ( "$apiKeyDbKey", Lantern.Query.String apiKeyDbKey )
                                    , ( "$apiKey", Lantern.Query.String appKey )
                                    ]
                                )
                                ConfigUpdated
                                |> Lantern.App.call
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Input ({ apiCredentials } as uiState) ->
                    refreshCache apiCredentials { model | words = parseWords model.userInput, uiState = Cards uiState }

                Cards uiState ->
                    ( { model | uiState = Export uiState }, Cmd.none )

                Export uiState ->
                    ( { model | uiState = Input uiState }, Cmd.none )

        UpdateApiCredentials apiCredentials ->
            case model.uiState of
                Config _ ->
                    ( { model | uiState = Config { apiCredentials = Just apiCredentials } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ConfigUpdated _ ->
            ( model, Cmd.none )

        LoadedCredentials result ->
            let
                default =
                    { appKey = "", appId = "" }

                apiCredentials =
                    case result of
                        Err _ ->
                            default

                        Ok credentials ->
                            credentials
                                |> List.foldl
                                    (\( name, value ) c ->
                                        if name == appIdDbKey then
                                            { c | appId = value }

                                        else if name == apiKeyDbKey then
                                            { c | appKey = value }

                                        else
                                            c
                                    )
                                    default
            in
            if String.isEmpty apiCredentials.appKey || String.isEmpty apiCredentials.appKey then
                ( { model | uiState = Config { apiCredentials = Just apiCredentials } }, Cmd.none )

            else
                ( { model | uiState = Input { apiCredentials = apiCredentials } }, Cmd.none )

        DownloadCards ->
            let
                flashCards =
                    model.words
                        |> List.filterMap
                            (\word ->
                                Dict.get word model.cache
                                    |> Maybe.andThen
                                        (\{ definition } ->
                                            case definition of
                                                Loaded def ->
                                                    Just def

                                                _ ->
                                                    Nothing
                                        )
                                    |> Maybe.map (renderFlashCardForExport model.userTranslations)
                            )
                        |> List.map (\{ front, back } -> [ Html.String.toString 0 front, Html.String.toString 0 back ])
            in
            ( model
            , File.Download.string "flashcards.csv" "text/csv" (Csv.Encode.toString { headers = [ "Front", "Back" ], records = flashCards })
                |> Cmd.map Lantern.App.Message
            )

        FuzzySelectMessage id foreignMsg ->
            ( { model
                | fuzzySelect =
                    model.fuzzySelect
                        |> Tuple.mapSecond (LanternUi.FuzzySelect.update foreignMsg)
                        |> Tuple.mapFirst (always id)
              }
            , Cmd.none
            )


flashCard : LanternUi.Theme.Theme -> ( String, FuzzySelect ) -> UserTranslations -> Word -> Element (Lantern.App.Message Message)
flashCard theme fuzzySelectState userTranslations { id, definition, translation } =
    let
        _ =
            Debug.log "translation" translation

        content =
            case definition of
                Loading ->
                    { front = Element.text id, back = Element.text "Loading..." }

                Loaded def ->
                    renderFlashCard
                        theme
                        { userTranslations = userTranslations
                        , definition = def
                        , fuzzySelectState = fuzzySelectState
                        , translation =
                            case translation of
                                Loaded tr ->
                                    tr

                                _ ->
                                    { word = id, translationEntries = [] }
                        }

                Errored msg ->
                    { front = Element.text id, back = Element.text ("Error: " ++ msg) }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.paragraph [ Element.width (Element.fillPortion 1), Element.alignTop ] [ content.front ]
        , Element.paragraph [ Element.width (Element.fillPortion 1), Element.alignTop ] [ content.back ]
        ]


flashCardForExport : UserTranslations -> Word -> Element (Lantern.App.Message Message)
flashCardForExport userTranslations { definition } =
    let
        content =
            case definition of
                Loaded def ->
                    Just (renderFlashCardForExport userTranslations def)

                _ ->
                    Nothing
    in
    case content of
        Just { front, back } ->
            Element.row
                [ Element.width Element.fill ]
                [ Element.paragraph [ Element.width (Element.fillPortion 1), Element.alignTop ] [ front |> Html.String.toHtml |> Element.html ]
                , Element.paragraph [ Element.width (Element.fillPortion 1), Element.alignTop ] [ back |> Html.String.toHtml |> Element.html ]
                ]

        Nothing ->
            Element.none


view : Context -> Model -> Element (Lantern.App.Message Message)
view { theme } { fuzzySelect, userTranslations, uiState, userInput, words, cache } =
    case uiState of
        Config { apiCredentials } ->
            case apiCredentials of
                Nothing ->
                    Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.centerX
                        , Element.centerY
                        ]
                        (Element.text "Loading...")

                Just { appId, appKey } ->
                    Element.column
                        [ Element.width Element.fill ]
                        [ LanternUi.Input.text
                            theme
                            []
                            { onChange = \value -> UpdateApiCredentials { appId = value, appKey = appKey } |> Lantern.App.Message
                            , text = appId
                            , placeholder = Just (Element.Input.placeholder [] (Element.text "App ID"))
                            , label = Element.Input.labelHidden "App ID"
                            }
                        , LanternUi.Input.text
                            theme
                            []
                            { onChange = \value -> UpdateApiCredentials { appId = appId, appKey = value } |> Lantern.App.Message
                            , text = appKey
                            , placeholder = Just (Element.Input.placeholder [] (Element.text "API Key"))
                            , label = Element.Input.labelHidden "API Key"
                            }
                        , LanternUi.Input.button theme [] { onPress = Just (Lantern.App.Message NextStep), label = Element.text "Next" }
                        ]

        Input _ ->
            Element.column
                [ Element.width Element.fill ]
                [ LanternUi.Input.multiline theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateUserInput >> Lantern.App.Message
                    , text = userInput
                    , placeholder = Just (Element.Input.placeholder [] (Element.text "word #1"))
                    , spellcheck = False
                    , label = Element.Input.labelHidden "Input Words"
                    }
                , LanternUi.Input.button theme
                    []
                    { onPress = Just (Lantern.App.Message NextStep)
                    , label = Element.text "Generate"
                    }
                ]

        Cards _ ->
            Element.column
                [ Element.width Element.fill ]
                [ words
                    |> List.filterMap (\word -> Dict.get word cache |> Maybe.map (flashCard theme fuzzySelect userTranslations))
                    |> Element.column [ Element.width Element.fill ]
                , LanternUi.Input.button theme
                    []
                    { onPress = Just (Lantern.App.Message NextStep)
                    , label = Element.text "Preview"
                    }
                ]

        Export _ ->
            Element.column
                [ Element.width Element.fill ]
                [ words
                    |> List.filterMap (\word -> Dict.get word cache |> Maybe.map (flashCardForExport userTranslations))
                    |> Element.column [ Element.width Element.fill ]
                , LanternUi.Input.button theme
                    []
                    { onPress = Just (Lantern.App.Message DownloadCards)
                    , label = Element.text "Export"
                    }
                ]


lanternApp : Lantern.App.App Context Model Message
lanternApp =
    Lantern.App.app
        { name = "Flashcards"
        , init = init
        , view = view
        , update = update
        , liveQueries = Nothing
        , subscriptions = always Sub.none
        }
