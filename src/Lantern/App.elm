module Lantern.App exposing (Message(..), mapMessage, unwrapMessage)

import Lantern


type Message msg
    = LanternMessage (Lantern.Message msg)
    | AppMessage msg


unwrapMessage : (Lantern.Message msg -> rootMsg) -> (msg -> rootMsg) -> Message msg -> rootMsg
unwrapMessage wrapLantern wrapApp msg =
    case msg of
        LanternMessage lanternMsg ->
            wrapLantern lanternMsg

        AppMessage appMsg ->
            wrapApp appMsg


mapMessage : (a -> b) -> Message a -> Message b
mapMessage f msg =
    case msg of
        LanternMessage lanternMsg ->
            LanternMessage (Lantern.map f lanternMsg)

        AppMessage appMsg ->
            AppMessage (f appMsg)
