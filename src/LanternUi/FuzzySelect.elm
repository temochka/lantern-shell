module LanternUi.FuzzySelect exposing (FuzzySelect, Message, new, render, reset, setId, update)

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


type alias FuzzySelect a =
    { options : List ( String, a )
    , cursor : Int
    , matches : List ( String, a )
    , query : String
    , active : Bool
    , placeholder : Maybe String
    , id : Maybe String
    }


new : { options : List ( String, a ), placeholder : Maybe String } -> FuzzySelect a
new { options, placeholder } =
    { options = options
    , matches = options
    , cursor = 0
    , query = ""
    , placeholder = placeholder
    , active = False
    , id = Nothing
    }


type Message a
    = UpdateQuery String
    | SetCursor Int
    | MoveCursorDown
    | MoveCursorUp
    | Activate
    | Deactivate
    | Nop


setId : String -> FuzzySelect a -> FuzzySelect a
setId id select =
    { select | id = Just id }


update : Message a -> FuzzySelect a -> FuzzySelect a
update msg model =
    case msg of
        UpdateQuery query ->
            { model
                | active = True
                , query = query
                , cursor = 0
                , matches = model.options |> List.filter (\( opt, _ ) -> String.contains (String.toLower query) (String.toLower opt))
            }

        SetCursor cursor ->
            { model | cursor = cursor }

        Activate ->
            { model | active = True }

        Deactivate ->
            { model | active = False }

        MoveCursorDown ->
            let
                newCursor =
                    min (model.cursor + 1) (List.length model.matches - 1)
            in
            { model | cursor = newCursor }

        MoveCursorUp ->
            let
                newCursor =
                    max 0 (model.cursor - 1)
            in
            { model | cursor = newCursor }

        Nop ->
            model


reset : FuzzySelect a -> FuzzySelect a
reset select =
    { select | active = False, query = "", cursor = 0, matches = select.options }


selectedMatch : FuzzySelect a -> Maybe a
selectedMatch { cursor, matches } =
    matches
        |> List.drop cursor
        |> List.head
        |> Maybe.map Tuple.second


render : Theme -> FuzzySelect a -> (Message a -> msg) -> (a -> msg) -> Element msg
render theme select toMsg onSelected =
    let
        handleKeyPress event =
            case event.keyCode of
                Keyboard.Key.Up ->
                    toMsg MoveCursorUp

                Keyboard.Key.Down ->
                    toMsg MoveCursorDown

                Keyboard.Key.Escape ->
                    toMsg Deactivate

                Keyboard.Key.Enter ->
                    selectedMatch select
                        |> Maybe.map onSelected
                        |> Maybe.withDefault (toMsg Nop)

                _ ->
                    toMsg Nop

        suggestions =
            (if select.active then
                select.matches
                    |> List.indexedMap
                        (\i ( description, option ) ->
                            let
                                bgColor =
                                    if i == select.cursor then
                                        theme.bgHighlight

                                    else
                                        theme.bgDefault
                            in
                            Element.el
                                [ Element.Background.color bgColor
                                , Element.pointer
                                , Element.width Element.fill
                                , Element.htmlAttribute (Html.Events.onMouseEnter (toMsg (SetCursor i)))
                                , Element.Events.onMouseDown (onSelected option)
                                ]
                                (Element.text description)
                        )

             else
                []
            )
                |> Element.column [ Element.width Element.fill ]
    in
    Element.Input.text
        [ Element.width Element.fill
        , Element.below suggestions
        , Element.Events.onFocus (toMsg Activate)
        , Element.Events.onLoseFocus (toMsg Deactivate)
        , Element.htmlAttribute (Html.Events.on "keydown" (Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent))
        , select.id |> Maybe.map (Html.Attributes.id >> Element.htmlAttribute) |> Maybe.withDefault LanternUi.noneAttribute
        ]
        { onChange = UpdateQuery >> toMsg, text = select.query, placeholder = select.placeholder |> Maybe.map (Element.text >> Element.Input.placeholder []), label = Element.Input.labelHidden "Query" }
