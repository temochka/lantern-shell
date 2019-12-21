module LanternUi.WindowManager exposing (Message, WindowManager, new, render, syncProcesses, update)

import Element exposing (Element)
import Element.Events
import Html.Attributes
import ProcessTable
import Set


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


new : List ProcessTable.Pid -> WindowManager
new runningPids =
    let
        sortedPids =
            List.sortBy negate runningPids
    in
    { focus = List.head sortedPids
    , layout = MasterStack (List.head sortedPids) (List.drop 1 sortedPids)
    }


syncProcesses : List ProcessTable.Pid -> WindowManager -> WindowManager
syncProcesses runningPids ({ layout, focus } as windowManager) =
    let
        layoutToPids =
            case layout of
                Stack windows ->
                    windows

                MasterStack masterWindow windows ->
                    (masterWindow |> Maybe.map List.singleton |> Maybe.withDefault []) ++ windows

        pidsToLayout pids =
            case layout of
                Stack _ ->
                    Stack pids

                MasterStack _ _ ->
                    MasterStack (List.head pids) (List.drop 1 pids)

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
                    List.sortBy negate (Set.toList pmPids) ++ accPids

        newWmPids =
            refreshPids layoutToPids (Set.fromList runningPids) []

        newFocus =
            focus
                |> Maybe.andThen
                    (\f ->
                        if List.member f newWmPids then
                            Just f

                        else
                            List.head newWmPids
                    )
    in
    { windowManager | layout = pidsToLayout newWmPids, focus = newFocus }


update : Message -> WindowManager -> WindowManager
update msg windowManager =
    case msg of
        Nop ->
            windowManager

        Focus pid ->
            { windowManager | focus = Just pid }


render : { spacing : Int, padding : Int } -> (ProcessTable.Pid -> Bool -> Element msg) -> (Message -> msg) -> WindowManager -> Element msg
render { spacing, padding } renderer wrapMsg { focus, layout } =
    let
        windowPane pid =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.onMouseEnter (wrapMsg (Focus pid))
                , Element.Events.onFocus (wrapMsg (Focus pid))
                , Element.htmlAttribute (Html.Attributes.tabindex 0)
                ]
                (renderer pid (focus |> Maybe.map ((==) pid) |> Maybe.withDefault False))
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
                ]
                [ Element.el
                    [ Element.width (Element.fillPortion 3)
                    , Element.height Element.fill
                    , Element.spacing spacing
                    ]
                    masterWindowPane
                , Element.column
                    [ Element.width (Element.fillPortion 2)
                    , Element.height Element.fill
                    , Element.spacing spacing
                    , Element.alignTop
                    ]
                    windowPanes
                ]
