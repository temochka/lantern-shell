module ProcessTable exposing (Pid, Process, ProcessTable, empty, kill, launch, lookup, pids, processApp, processes)

import Dict exposing (Dict)


type alias Arg =
    { name : String
    }


type alias Process app =
    { application : app
    , arguments : Dict String String
    , pid : Pid
    }


type alias Launcher app =
    app -> List Arg -> Process app


type alias Pid =
    Int


type alias ProcessTable app =
    { pid : Pid
    , processes : Dict Pid (Process app)
    }


type Message app
    = Launch (Launcher app)


empty : ProcessTable app
empty =
    { processes = Dict.empty
    , pid = 0
    }


processes : ProcessTable app -> List (Process app)
processes table =
    Dict.values table.processes


pids : ProcessTable app -> List Pid
pids table =
    Dict.keys table.processes


processApp : Process app -> app
processApp process =
    process.application


lookup : ProcessTable app -> Pid -> Maybe (Process app)
lookup table pid =
    Dict.get pid table.processes


launch : app -> ProcessTable app -> ProcessTable app
launch app processTable =
    let
        pid =
            processTable.pid + 1

        newProcess =
            { application = app
            , arguments = Dict.empty
            , pid = pid
            }
    in
    { processTable | pid = pid, processes = Dict.insert pid newProcess processTable.processes }


kill : Pid -> ProcessTable app -> ProcessTable app
kill pid table =
    { table | processes = Dict.remove pid table.processes }
