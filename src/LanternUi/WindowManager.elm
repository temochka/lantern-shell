module LanternUi.WindowManager exposing
    ( Message(..)
    , RenderOptions
    , WindowManager
    , deserialize
    , new
    , nextWindow
    , prevWindow
    , render
    , serialize
    , syncProcesses
    , update
    )

import Base64
import Browser.Dom
import Element exposing (Element)
import Element.Events
import Html.Attributes
import Json.Decode
import Json.Encode
import ProcessTable
import Set
import Task


type Layout
    = Stack (List ProcessTable.Pid)
    | MasterStack (Maybe ProcessTable.Pid) (List ProcessTable.Pid)


type alias WindowManager =
    { focus : Maybe ProcessTable.Pid
    , layout : Layout
    }


type Message
    = Nop
    | Focus ProcessTable.Pid
    | SyncProcesses (List ProcessTable.Pid)
    | NextWindow
    | PrevWindow


type alias RenderOptions =
    { pid : ProcessTable.Pid
    , focused : Bool
    , id : String
    , tabindex : Int
    }


new : List ProcessTable.Pid -> WindowManager
new runningPids =
    let
        sortedPids =
            List.sortBy negate runningPids
    in
    { focus = List.head sortedPids
    , layout = MasterStack (List.head sortedPids) (List.drop 1 sortedPids)
    }


layoutToPids layout =
    case layout of
        Stack windows ->
            windows

        MasterStack masterWindow windows ->
            (masterWindow |> Maybe.map List.singleton |> Maybe.withDefault []) ++ windows


pidsToLayout pids layout =
    case layout of
        Stack _ ->
            Stack pids

        MasterStack _ _ ->
            MasterStack (List.head pids) (List.drop 1 pids)


syncProcesses : List ProcessTable.Pid -> WindowManager -> ( WindowManager, Cmd Message )
syncProcesses runningPids ({ layout, focus } as windowManager) =
    let
        refreshPids wmPids pmPids accPids =
            case wmPids of
                pid :: rest ->
                    (if Set.member pid pmPids then
                        pid :: accPids

                     else
                        accPids
                    )
                        |> refreshPids rest (Set.remove pid pmPids)

                [] ->
                    List.sortBy negate (Set.toList pmPids) ++ List.reverse accPids

        currentWmPids =
            layoutToPids layout

        newWmPids =
            refreshPids currentWmPids (Set.fromList runningPids) []

        newFocus =
            if List.head newWmPids == List.head currentWmPids then
                focus
                    |> Maybe.andThen
                        (\f ->
                            if List.member f newWmPids then
                                Just f

                            else
                                Nothing
                        )
                    |> (\f ->
                            if f == Nothing then
                                List.head newWmPids

                            else
                                f
                       )

            else
                List.head newWmPids
    in
    ( { windowManager | layout = pidsToLayout newWmPids layout, focus = newFocus }
    , if newFocus /= focus then
        newFocus |> Maybe.map (\pid -> pid |> windowId windowManager |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)) |> Maybe.withDefault Cmd.none

      else
        Cmd.none
    )


layoutName : Layout -> String
layoutName layout =
    case layout of
        Stack _ ->
            "Stack"

        MasterStack _ _ ->
            "MasterStack"


serialize : (ProcessTable.Pid -> { processName : String, flags : Maybe Json.Encode.Value }) -> WindowManager -> String
serialize serializeProcess { layout } =
    Json.Encode.object
        [ ( "l", Json.Encode.string (layoutName layout) )
        , ( "a"
          , Json.Encode.list
                (serializeProcess
                    >> (\{ processName, flags } ->
                            Json.Encode.list identity
                                (Json.Encode.string processName
                                    :: (flags |> Maybe.map List.singleton |> Maybe.withDefault [])
                                )
                       )
                )
                (layoutToPids layout)
          )
        ]
        |> Json.Encode.encode 0
        |> Base64.encode


deserialize : (( String, Maybe Json.Decode.Value ) -> Maybe launcher) -> String -> Maybe ( WindowManager, List launcher )
deserialize toLauncher serialized =
    let
        toLayout name =
            (case name of
                "Stack" ->
                    Just (Stack [])

                "MasterStack" ->
                    Just (MasterStack Nothing [])

                _ ->
                    Nothing
            )
                |> Maybe.map (\layout -> { focus = Nothing, layout = layout })

        layoutDecoder =
            Json.Decode.field "l" Json.Decode.string
                |> Json.Decode.map toLayout

        processesDecoder =
            Json.Decode.field "a"
                (Json.Decode.list
                    (Json.Decode.map2 Tuple.pair
                        (Json.Decode.index 0 Json.Decode.string)
                        (Json.Decode.maybe (Json.Decode.index 1 Json.Decode.value))
                    )
                )
                |> Json.Decode.map (List.filterMap toLauncher >> List.reverse >> Just)

        jsonDecoder =
            Json.Decode.map2 (Maybe.map2 Tuple.pair)
                layoutDecoder
                processesDecoder
    in
    serialized
        |> Base64.decode
        |> Result.toMaybe
        |> Maybe.andThen (Json.Decode.decodeString jsonDecoder >> Result.toMaybe)
        |> Maybe.andThen identity


update : Message -> WindowManager -> ( WindowManager, Cmd Message )
update msg windowManager =
    case msg of
        Nop ->
            ( windowManager, Cmd.none )

        Focus pid ->
            ( { windowManager | focus = Just pid }, Cmd.none )

        NextWindow ->
            nextWindow windowManager

        PrevWindow ->
            prevWindow windowManager

        SyncProcesses pids ->
            syncProcesses pids windowManager


windowId : WindowManager -> ProcessTable.Pid -> String
windowId windowManager pid =
    "window-" ++ String.fromInt pid


render :
    { spacing : Int, padding : Int }
    -> (RenderOptions -> Element msg)
    -> (Message -> msg)
    -> WindowManager
    -> Element msg
render { spacing, padding } renderer wrapMsg ({ focus, layout } as windowManager) =
    let
        windowPane pid =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.onMouseEnter (wrapMsg (Focus pid))
                , Element.Events.onFocus (wrapMsg (Focus pid))
                ]
                (renderer
                    { pid = pid
                    , focused = focus |> Maybe.map ((==) pid) |> Maybe.withDefault False
                    , tabindex = 0
                    , id = windowId windowManager pid
                    }
                )
    in
    case layout of
        Stack windows ->
            let
                windowPanes =
                    windows
                        |> List.map windowPane
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing spacing
                , Element.padding padding
                , Element.clip
                ]
                windowPanes

        MasterStack masterWindow windows ->
            let
                masterWindowPane =
                    masterWindow |> Maybe.map windowPane |> Maybe.withDefault Element.none

                windowPanes =
                    List.map windowPane windows
            in
            Element.row
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding padding
                , Element.spacing spacing
                , Element.clip
                ]
                [ Element.el
                    [ Element.width (Element.fillPortion 3)
                    , Element.height Element.fill
                    , Element.spacing (3 + spacing)
                    , Element.padding 3
                    , Element.clip
                    , Element.scrollbarY
                    ]
                    masterWindowPane
                , if not (List.isEmpty windowPanes) then
                    Element.column
                        [ Element.width (Element.fillPortion 2)
                        , Element.height Element.fill
                        , Element.spacing (6 + spacing)
                        , Element.alignTop
                        , Element.padding 3
                        , Element.clip
                        , Element.scrollbarY
                        ]
                        windowPanes

                  else
                    Element.none
                ]


nextWindow : WindowManager -> ( WindowManager, Cmd Message )
nextWindow ({ focus, layout } as windowManager) =
    let
        pids =
            layoutToPids layout

        findNext remainingPids focusedPid =
            case remainingPids of
                pid :: rest ->
                    if pid == focusedPid then
                        List.head rest

                    else
                        findNext rest focusedPid

                _ ->
                    Nothing

        newFocus =
            focus
                |> Maybe.andThen (findNext pids)
                |> (\f ->
                        case f of
                            Just pid ->
                                Just pid

                            Nothing ->
                                List.head pids
                   )
    in
    ( { windowManager | focus = newFocus }
    , newFocus |> Maybe.map (\pid -> pid |> windowId windowManager |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)) |> Maybe.withDefault Cmd.none
    )


prevWindow : WindowManager -> ( WindowManager, Cmd Message )
prevWindow ({ focus, layout } as windowManager) =
    let
        pids =
            layoutToPids layout

        findPrev remainingPids focusedPid =
            case remainingPids of
                prevPid :: pid :: rest ->
                    if pid == focusedPid then
                        Just prevPid

                    else if prevPid == focusedPid then
                        List.reverse pids |> List.head

                    else
                        findPrev (pid :: rest) focusedPid

                _ ->
                    Nothing

        newFocus =
            focus
                |> Maybe.andThen (findPrev pids)
                |> Maybe.map Just
                |> Maybe.withDefault (List.head pids)
    in
    ( { windowManager | focus = newFocus }
    , newFocus |> Maybe.map (\pid -> pid |> windowId windowManager |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)) |> Maybe.withDefault Cmd.none
    )
