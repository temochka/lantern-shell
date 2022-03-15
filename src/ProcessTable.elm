module ProcessTable exposing (Pid, Process, ProcessTable, empty, kill, launch, lookup, mapProcess, pids, processApp, processes, processesWithPids, userPids)

import Dict exposing (Dict)


type alias Arg =
    { name : String
    }


type alias Process app =
    { application : app
    , name : String
    , arguments : List Arg
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


empty : ProcessTable app
empty =
    { processes = Dict.empty

    -- First launched process should be 0
    , pid = -1
    }


processes : ProcessTable app -> List (Process app)
processes table =
    Dict.values table.processes


processesWithPids : ProcessTable app -> List ( Pid, Process app )
processesWithPids table =
    Dict.toList table.processes


pids : ProcessTable app -> List Pid
pids table =
    Dict.keys table.processes


userPids : ProcessTable app -> List Pid
userPids table =
    table.processes |> Dict.remove 0 |> Dict.keys


processApp : Process app -> app
processApp process =
    process.application


lookup : ProcessTable app -> Pid -> Maybe (Process app)
lookup table pid =
    Dict.get pid table.processes


launch : app -> { name : String, arguments : List Arg } -> ProcessTable app -> ProcessTable app
launch app { name, arguments } processTable =
    let
        pid =
            processTable.pid + 1

        newProcess =
            { application = app
            , arguments = arguments
            , name = name
            , pid = pid
            }
    in
    { processTable | pid = pid, processes = Dict.insert pid newProcess processTable.processes }


kill : Pid -> ProcessTable app -> ProcessTable app
kill pid table =
    { table | processes = Dict.remove pid table.processes }


mapProcess : (app -> app) -> Pid -> ProcessTable app -> ProcessTable app
mapProcess f pid processTable =
    { processTable
        | processes = Dict.update pid (Maybe.map (mapApp f)) processTable.processes
    }


mapApp : (app -> app) -> Process app -> Process app
mapApp f ({ application } as process) =
    { process | application = f application }
