module LanternUi.FuzzySelect exposing (FuzzySelect, Message, fuzzySelect, update)

import Element exposing (Element)
import Element.Background
import Element.Events
import Element.Input
import Html.Attributes
import Html.Events
import Json.Decode
import Keyboard.Event
import Keyboard.Key
import LanternUi
import LanternUi.Theme exposing (Theme)


type alias FuzzySelect =
    Maybe { cursor : Int }


init : FuzzySelect
init =
    Just { cursor = 0 }


type Message
    = SetCursor Int
    | Toggle FuzzySelect
    | MoveCursorDown Int
    | MoveCursorUp
    | Nop


update : Message -> FuzzySelect -> FuzzySelect
update msg maybeModel =
    case msg of
        SetCursor cursor ->
            Maybe.map (\model -> { model | cursor = cursor }) maybeModel

        MoveCursorDown maxCursor ->
            Maybe.map (\model -> { model | cursor = min maxCursor (model.cursor + 1) }) maybeModel

        MoveCursorUp ->
            Maybe.map (\model -> { model | cursor = max 0 (model.cursor - 1) }) maybeModel

        Toggle model ->
            model

        Nop ->
            maybeModel


selectedMatch : Int -> List ( String, a ) -> Maybe a
selectedMatch cursor matches =
    matches
        |> List.drop (min cursor (List.length matches - 1))
        |> List.head
        |> Maybe.map Tuple.second


fuzzySelect :
    Theme
    ->
        { label : Element.Input.Label msg
        , onQueryChange : String -> msg
        , onOptionSelect : a -> msg
        , onInternalMessage : Message -> msg
        , options : List ( String, a )
        , placeholder : Maybe (Element.Input.Placeholder msg)
        , query : String
        , state : FuzzySelect
        , id : Maybe String
        }
    -> Element msg
fuzzySelect theme { label, onQueryChange, onInternalMessage, onOptionSelect, options, placeholder, query, state, id } =
    let
        matches =
            options
                |> List.filter
                    (\( opt, _ ) ->
                        String.contains (String.toLower query) (String.toLower opt)
                    )

        handleKeyPress event =
            case event.keyCode of
                Keyboard.Key.Up ->
                    onInternalMessage MoveCursorUp

                Keyboard.Key.Down ->
                    onInternalMessage (MoveCursorDown (List.length options - 1))

                Keyboard.Key.Escape ->
                    onInternalMessage (Toggle Nothing)

                Keyboard.Key.Enter ->
                    state
                        |> Maybe.andThen
                            (\{ cursor } ->
                                selectedMatch cursor matches
                            )
                        |> Maybe.map onOptionSelect
                        |> Maybe.withDefault (onInternalMessage Nop)

                _ ->
                    onInternalMessage Nop

        suggestions =
            (case state of
                Just { cursor } ->
                    let
                        cappedCursor =
                            min cursor (List.length matches - 1)
                    in
                    matches
                        |> List.indexedMap
                            (\i ( description, option ) ->
                                let
                                    bgColor =
                                        if i == cappedCursor then
                                            theme.bgHighlight

                                        else
                                            theme.bgDefault
                                in
                                Element.el
                                    [ Element.Background.color bgColor
                                    , Element.pointer
                                    , Element.width Element.fill
                                    , Element.paddingXY 10 5
                                    , Element.htmlAttribute (Html.Events.onMouseEnter (onInternalMessage (SetCursor i)))
                                    , Element.Events.onMouseDown (onOptionSelect option)
                                    ]
                                    (Element.text description)
                            )

                Nothing ->
                    []
            )
                |> Element.column [ Element.width Element.fill ]
    in
    Element.Input.text
        [ Element.width Element.fill
        , Element.below suggestions
        , Element.Events.onFocus (onInternalMessage (Toggle init))
        , Element.Events.onLoseFocus (onInternalMessage (Toggle Nothing))
        , Element.htmlAttribute (Html.Events.on "keydown" (Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent))
        , id |> Maybe.map (Html.Attributes.id >> Element.htmlAttribute) |> Maybe.withDefault LanternUi.noneAttribute
        ]
        { onChange = onQueryChange
        , text = query
        , placeholder = placeholder
        , label = label
        }
