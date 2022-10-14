module Lantern.App exposing (App, Message(..), app, call, liveApp, mount, simpleApp)

import Element exposing (Element)
import Lantern
import Lantern.LiveQuery exposing (LiveQuery)
import Task exposing (Task)


type Message msg
    = Message msg
    | LanternMessage (Lantern.Message msg)


mapMessage : (msgA -> msgB) -> Message msgA -> Message msgB
mapMessage f msg =
    case msg of
        Message appMessage ->
            Message (f appMessage)

        LanternMessage lanternMessage ->
            LanternMessage (Lantern.map f lanternMessage)


call : Task Never (Lantern.Message msg) -> Cmd (Message msg)
call task =
    Task.perform LanternMessage task


type alias App ctx flags model msg =
    { init : Maybe flags -> ( model, Cmd (Message msg) )
    , view : ctx -> model -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    , liveQueries : model -> List (LiveQuery msg)
    , subscriptions : model -> Sub (Message msg)
    , name : String
    }


liveApp :
    { name : String
    , init : model
    , view :
        ctx
        -> model
        -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    , liveQueries : model -> List (LiveQuery msg)
    , subscriptions : model -> Sub (Message msg)
    }
    -> App ctx flags model msg
liveApp def =
    { name = def.name
    , init = \_ -> ( def.init, Cmd.none )
    , view = def.view
    , update = def.update
    , liveQueries = def.liveQueries
    , subscriptions = def.subscriptions
    }


app :
    { name : String
    , init : Maybe flags -> ( model, Cmd (Message msg) )
    , view :
        ctx
        -> model
        -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    , liveQueries : Maybe (model -> List (LiveQuery msg))
    , subscriptions : model -> Sub (Message msg)
    }
    -> App ctx flags model msg
app def =
    { name = def.name
    , init = def.init
    , view = def.view
    , update = def.update
    , liveQueries = def.liveQueries |> Maybe.withDefault (always [])
    , subscriptions = def.subscriptions
    }


simpleApp :
    { name : String
    , init : model
    , view :
        ctx
        -> model
        -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    }
    -> App ctx flags model msg
simpleApp def =
    { name = def.name
    , init = \_ -> ( def.init, Cmd.none )
    , view = def.view
    , update = def.update
    , liveQueries = always []
    , subscriptions = always Sub.none
    }


mount :
    { unwrapMsg : rootMsg -> Maybe appMsg
    , wrapMsg : appMsg -> rootMsg
    , unwrapModel : rootModel -> Maybe appModel
    , wrapModel : appModel -> rootModel
    , flags : Maybe appFlags
    , context : rootModel -> ctx
    }
    -> App ctx appFlags appModel appMsg
    -> App () rootFlags rootModel rootMsg
mount { unwrapMsg, wrapMsg, unwrapModel, wrapModel, flags, context } mountedApp =
    let
        wrapResult ( appModel, appCmd ) =
            ( wrapModel appModel, Cmd.map (mapMessage wrapMsg) appCmd )

        wrappedUpdate rootMsg rootModel =
            Maybe.map2
                (\appMsg appModel ->
                    mountedApp.update appMsg appModel |> wrapResult
                )
                (unwrapMsg rootMsg)
                (unwrapModel rootModel)
                |> Maybe.withDefault ( rootModel, Cmd.none )

        wrappedView _ rootModel =
            rootModel
                |> unwrapModel
                |> Maybe.map (mountedApp.view (context rootModel) >> Element.map (mapMessage wrapMsg))
                |> Maybe.withDefault Element.none

        wrappedLiveQueries rootModel =
            rootModel
                |> unwrapModel
                |> Maybe.map (mountedApp.liveQueries >> List.map (Lantern.LiveQuery.map wrapMsg))
                |> Maybe.withDefault []

        wrappedSubscriptions rootModel =
            rootModel
                |> unwrapModel
                |> Maybe.map (mountedApp.subscriptions >> Sub.map (mapMessage wrapMsg))
                |> Maybe.withDefault Sub.none
    in
    { name = mountedApp.name
    , init = \_ -> mountedApp.init flags |> wrapResult
    , view = wrappedView
    , update = wrappedUpdate
    , liveQueries = wrappedLiveQueries
    , subscriptions = wrappedSubscriptions
    }
