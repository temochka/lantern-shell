module LanternUi.WindowManager exposing (Message, WindowManager, new, render, syncProcesses, update)

import Element exposing (Element)
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

        refreshPids wmPids pmPids =
            case wmPids of
                pid :: rest ->
                    if Set.member pid pmPids then
                        pid :: refreshPids rest (Set.remove pid pmPids)

                    else
                        refreshPids rest pmPids

                [] ->
                    Set.toList pmPids

        newWmPids =
            refreshPids layoutToPids (Set.fromList runningPids)

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


render : (ProcessTable.Pid -> Element msg) -> WindowManager -> Element msg
render renderer { focus, layout } =
    let
        toWindowPane =
            renderer >> windowPane
    in
    case layout of
        Stack windows ->
            let
                windowPanes =
                    windows
                        |> List.map toWindowPane
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                windowPanes

        MasterStack masterWindow windows ->
            let
                masterWindowPane =
                    masterWindow |> Maybe.map toWindowPane |> Maybe.withDefault Element.none

                windowPanes =
                    List.map toWindowPane windows
            in
            Element.row
                [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.el [ Element.width (Element.fillPortion 3), Element.clip, Element.height Element.fill ] masterWindowPane
                , Element.column [ Element.width (Element.fillPortion 2), Element.clip ] windowPanes
                ]


windowPane : Element msg -> Element msg
windowPane content =
    Element.el
        [ Element.width Element.fill, Element.height Element.fill ]
        content
