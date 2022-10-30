module LanternUi.WindowManager exposing
    ( Message(..)
    , RenderOptions
    , WindowId
    , WindowManager
    , WindowState
    , closeWindowsForPid
    , deserialize
    , new
    , newWindow
    , nextWindow
    , prevWindow
    , render
    , serialize
    , update
    )

import Base64
import Browser.Dom
import Dict
import Element exposing (Element)
import Element.Events
import Json.Decode
import Json.Encode
import ProcessTable
import Set
import Task


type Layout
    = Stack (List WindowId)
    | MasterStack (List WindowId)


type alias WindowId =
    Int


type alias WindowState =
    Json.Encode.Value


type alias Window =
    { pid : ProcessTable.Pid
    , id : WindowId
    , state : WindowState
    }


type alias WindowManager =
    { focus : Maybe WindowId
    , layout : Layout
    , windows : Dict.Dict Int Window
    , windowsPerPid : Dict.Dict ProcessTable.Pid (List WindowId)
    , maxWindowId : Int
    }


type Message
    = Nop
    | Focus WindowId
    | NewWindow ProcessTable.Pid WindowState
    | NextWindow
    | PrevWindow


type alias RenderOptions =
    { pid : ProcessTable.Pid
    , focused : Bool
    , id : String
    , tabindex : Int
    }


new : WindowManager
new =
    { focus = Nothing
    , layout = MasterStack []
    , windows = Dict.empty
    , windowsPerPid = Dict.empty
    , maxWindowId = 0
    }


newWindow : ProcessTable.Pid -> WindowState -> WindowManager -> WindowManager
newWindow pid state windowManager =
    let
        newWindowId =
            windowManager.maxWindowId + 1

        window =
            { pid = pid, id = newWindowId, state = state }

        newWindowsPerPid =
            Dict.update pid
                (\mWids ->
                    case mWids of
                        Just wids ->
                            Just (newWindowId :: wids)

                        Nothing ->
                            Just [ newWindowId ]
                )
                windowManager.windowsPerPid

        newLayout =
            case windowManager.layout of
                MasterStack windowIds ->
                    MasterStack <| (newWindowId :: windowIds)

                Stack windowIds ->
                    Stack <| (newWindowId :: windowIds)
    in
    { windowManager
        | focus = Just newWindowId
        , layout = newLayout
        , windows = Dict.insert newWindowId window windowManager.windows
        , windowsPerPid = newWindowsPerPid
        , maxWindowId = newWindowId
    }


closeWindowsForPid : ProcessTable.Pid -> WindowManager -> WindowManager
closeWindowsForPid pid windowManager =
    let
        affectedWindowIds =
            Dict.get pid windowManager.windowsPerPid |> Maybe.withDefault [] |> Set.fromList

        newWindowsPerPids =
            Dict.remove pid windowManager.windowsPerPid

        windowFilter =
            \id -> not (Set.member id affectedWindowIds)

        newWindows =
            Dict.filter (\k _ -> windowFilter k) windowManager.windows

        newLayout =
            mapLayout (List.filter windowFilter) windowManager.layout

        newFocus =
            windowManager.focus
                |> Maybe.andThen
                    (\windowId ->
                        if Set.member windowId affectedWindowIds then
                            newLayout |> layoutToWindowIds |> List.head

                        else
                            windowManager.focus
                    )
    in
    { windowManager
        | focus = newFocus
        , windows = newWindows
        , windowsPerPid = newWindowsPerPids
        , layout = newLayout
    }


layoutToWindowIds : Layout -> List ProcessTable.Pid
layoutToWindowIds layout =
    case layout of
        Stack windows ->
            windows

        MasterStack windows ->
            windows


mapLayout : (List WindowId -> List WindowId) -> Layout -> Layout
mapLayout f layout =
    case layout of
        Stack windowIds ->
            Stack <| f windowIds

        MasterStack windowIds ->
            MasterStack <| f windowIds


layoutName : Layout -> String
layoutName layout =
    case layout of
        Stack _ ->
            "Stack"

        MasterStack _ ->
            "MasterStack"


serialize : (ProcessTable.Pid -> String) -> WindowManager -> String
serialize serializeProcess { layout, windowsPerPid, windows } =
    let
        toSerialize =
            windowsPerPid
                |> Dict.toList
                |> List.map
                    (\( pid, windowIds ) ->
                        ( pid
                        , windowIds |> List.filterMap (\windowId -> Dict.get windowId windows) |> List.map .state
                        )
                    )
    in
    Json.Encode.object
        [ ( "l", Json.Encode.string (layoutName layout) )
        , ( "a"
          , Json.Encode.list
                (\( pid, windowStates ) ->
                    Json.Encode.list identity
                        [ serializeProcess pid |> Json.Encode.string
                        , windowStates |> Json.Encode.list identity
                        ]
                )
                toSerialize
          )
        ]
        |> Json.Encode.encode 0
        |> Base64.encode


deserialize : (String -> Maybe launcher) -> String -> Maybe ( WindowManager, List ( launcher, List WindowState ) )
deserialize toLauncher serialized =
    let
        toLayout name =
            (case name of
                "Stack" ->
                    Just (Stack [])

                "MasterStack" ->
                    Just (MasterStack [])

                _ ->
                    Nothing
            )
                |> Maybe.map (\layout -> { new | focus = Nothing, layout = layout })

        layoutDecoder =
            Json.Decode.field "l" Json.Decode.string
                |> Json.Decode.map toLayout

        processesDecoder =
            Json.Decode.field "a" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 (Json.Decode.list Json.Decode.value))))
                |> Json.Decode.map
                    (List.filterMap
                        (\( appName, windowStates ) ->
                            toLauncher appName |> Maybe.map (\l -> ( l, windowStates ))
                        )
                        >> List.reverse
                        >> Just
                    )

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

        Focus windowId ->
            ( { windowManager | focus = Just windowId }, Cmd.none )

        NextWindow ->
            nextWindow windowManager

        PrevWindow ->
            prevWindow windowManager

        NewWindow pid flags ->
            let
                newState =
                    newWindow pid flags windowManager
            in
            ( { newState | focus = Just newState.maxWindowId }
            , newState.maxWindowId |> String.fromInt |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)
            )


render :
    { spacing : Int, padding : Int }
    -> (RenderOptions -> Element msg)
    -> (Message -> msg)
    -> WindowManager
    -> Element msg
render { spacing, padding } renderer wrapMsg ({ focus, layout } as model) =
    let
        windowPane pid windowId =
            Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.Events.onMouseEnter (wrapMsg (Focus pid))
                , Element.Events.onFocus (wrapMsg (Focus pid))
                ]
                (renderer
                    { pid = pid
                    , focused = focus |> Maybe.map ((==) windowId) |> Maybe.withDefault False
                    , tabindex = 0
                    , id = String.fromInt windowId
                    }
                )
    in
    case layout of
        Stack windows ->
            let
                windowPanes =
                    windows
                        |> List.filterMap (\id -> Dict.get id model.windows)
                        |> List.map (\w -> windowPane w.pid w.id)
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing spacing
                , Element.padding padding
                , Element.clip
                ]
                windowPanes

        MasterStack windows ->
            let
                windowPanes =
                    windows
                        |> List.filterMap (\id -> Dict.get id model.windows)
                        |> List.map (\w -> windowPane w.pid w.id)

                ( masterPane, sidePanes ) =
                    Maybe.map2 Tuple.pair (List.head windowPanes) (List.tail windowPanes)
                        |> Maybe.withDefault ( Element.none, [] )
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
                    masterPane
                , if not (List.isEmpty sidePanes) then
                    Element.column
                        [ Element.width (Element.fillPortion 2)
                        , Element.height Element.fill
                        , Element.spacing (6 + spacing)
                        , Element.alignTop
                        , Element.padding 3
                        , Element.clip
                        , Element.scrollbarY
                        ]
                        sidePanes

                  else
                    Element.none
                ]


nextWindow : WindowManager -> ( WindowManager, Cmd Message )
nextWindow ({ focus, layout } as windowManager) =
    let
        windowIds =
            layoutToWindowIds layout

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
                |> Maybe.andThen (findNext windowIds)
                |> (\f ->
                        case f of
                            Just pid ->
                                Just pid

                            Nothing ->
                                List.head windowIds
                   )
    in
    ( { windowManager | focus = newFocus }
    , newFocus |> Maybe.map (\windowId -> windowId |> String.fromInt |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)) |> Maybe.withDefault Cmd.none
    )


prevWindow : WindowManager -> ( WindowManager, Cmd Message )
prevWindow ({ focus, layout } as windowManager) =
    let
        windowIds =
            layoutToWindowIds layout

        findPrev remainingPids focusedPid =
            case remainingPids of
                prevPid :: pid :: rest ->
                    if pid == focusedPid then
                        Just prevPid

                    else if prevPid == focusedPid then
                        List.reverse windowIds |> List.head

                    else
                        findPrev (pid :: rest) focusedPid

                _ ->
                    Nothing

        newFocus =
            focus
                |> Maybe.andThen (findPrev windowIds)
                |> Maybe.map Just
                |> Maybe.withDefault (List.head windowIds)
    in
    ( { windowManager | focus = newFocus }
    , newFocus |> Maybe.map (\windowId -> windowId |> String.fromInt |> Browser.Dom.focus |> Task.attempt (\_ -> Nop)) |> Maybe.withDefault Cmd.none
    )
