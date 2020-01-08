module Lantern.App exposing (App, liveApp, mount, simpleApp)

import Element exposing (Element)
import Lantern exposing (Message)
import Lantern.LiveQuery exposing (LiveQuery)


type alias App ctx model msg =
    { init : ( model, Cmd (Message msg) )
    , view : ctx -> model -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    , liveQueries : model -> List (LiveQuery msg)
    }


liveApp :
    { init : model
    , view :
        ctx
        -> model
        -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    , liveQueries : model -> List (LiveQuery msg)
    }
    -> App ctx model msg
liveApp def =
    { init = ( def.init, Cmd.none )
    , view = def.view
    , update = def.update
    , liveQueries = def.liveQueries
    }


simpleApp :
    { init : model
    , view :
        ctx
        -> model
        -> Element (Message msg)
    , update : msg -> model -> ( model, Cmd (Message msg) )
    }
    -> App ctx model msg
simpleApp def =
    { init = ( def.init, Cmd.none )
    , view = def.view
    , update = def.update
    , liveQueries = always []
    }


mount :
    { unwrapMsg : rootMsg -> Maybe appMsg
    , wrapMsg : appMsg -> rootMsg
    , unwrapModel : rootModel -> Maybe appModel
    , wrapModel : appModel -> rootModel
    , context : rootModel -> ctx
    }
    -> App ctx appModel appMsg
    -> App () rootModel rootMsg
mount { unwrapMsg, wrapMsg, unwrapModel, wrapModel, context } app =
    let
        wrapResult ( appModel, appCmd ) =
            ( wrapModel appModel, Cmd.map (Lantern.map wrapMsg) appCmd )

        wrappedUpdate rootMsg rootModel =
            Maybe.map2
                (\appMsg appModel ->
                    app.update appMsg appModel |> wrapResult
                )
                (unwrapMsg rootMsg)
                (unwrapModel rootModel)
                |> Maybe.withDefault ( rootModel, Cmd.none )

        wrappedView _ rootModel =
            rootModel
                |> unwrapModel
                |> Maybe.map (app.view (context rootModel) >> Element.map (Lantern.map wrapMsg))
                |> Maybe.withDefault Element.none

        wrappedLiveQueries rootModel =
            rootModel
                |> unwrapModel
                |> Maybe.map (app.liveQueries >> List.map (Lantern.LiveQuery.map wrapMsg))
                |> Maybe.withDefault []
    in
    { init = app.init |> wrapResult
    , view = wrappedView
    , update = wrappedUpdate
    , liveQueries = wrappedLiveQueries
    }
