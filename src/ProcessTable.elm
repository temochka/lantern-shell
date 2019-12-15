module ProcessTable exposing (ProcessTable, empty, launch, processApp, processes)

import Dict exposing (Dict)


type alias Arg =
    { name : String
    }


type alias Process app =
    { application : app
    , arguments : Dict String String
    }


type alias Launcher app =
    app -> List Arg -> Process app


type alias ProcessTable app =
    { launchers : List (Launcher app)
    , processes : List (Process app)
    }


type Message app
    = Launch (Launcher app)


empty : ProcessTable app
empty =
    { launchers = []
    , processes = []
    }


processes : ProcessTable app -> List (Process app)
processes table =
    table.processes


processApp : Process app -> app
processApp process =
    process.application


launch : ProcessTable app -> app -> ProcessTable app
launch processTable app =
    let
        newProcess =
            { application = app
            , arguments = Dict.empty
            }
    in
    { processTable | processes = newProcess :: processTable.processes }
