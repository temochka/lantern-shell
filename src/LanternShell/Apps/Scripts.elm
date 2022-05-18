module LanternShell.Apps.Scripts exposing (Message(..), Model, MyIO, Script, lanternApp, scriptsQuery)

import Array
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure
import Enclojure.Located as Located exposing (Located(..))
import Enclojure.Runtime as Runtime exposing (emptyCallable)
import Enclojure.Types exposing (Env, Exception(..), IO(..), Thunk(..), Value(..))
import Enclojure.ValueMap
import File.Download
import Html.Events
import Json.Decode
import Keyboard.Event
import Keyboard.Key exposing (Key(..))
import Lantern
import Lantern.App
import Lantern.Http
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternUi
import LanternUi.FuzzySelect exposing (FuzzySelect)
import LanternUi.Input
import LanternUi.Theme
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type TextFormat
    = Plain String
    | TextRef InputKey


type InputCell
    = TextInput { suggestions : List String }
    | MaskedTextInput
    | Button { title : String }
    | Download { name : String, contentType : String, content : String }


type alias InputKey =
    String


type Cell
    = Input InputKey InputCell
    | Text (List TextFormat)
    | VStack (List Cell)
    | HStack (List Cell)


type ConsoleEntry
    = ConsoleString String
    | UiTrace UiModel
    | Savepoint_ ( Located ( IO MyIO, Env MyIO ), Maybe (Thunk MyIO) )
    | Success (Value MyIO)
    | Failure ( Located Exception, Env MyIO )


type alias Console =
    List ConsoleEntry


type alias BrowserModel =
    { scripts : List Script
    , query : String
    }


type alias EditorModel =
    { interpreter : Interpreter
    , script : Script
    , console : Console
    , repl : String
    }


type Model
    = Browser BrowserModel
    | Editor EditorModel
    | Runner EditorModel


type alias Flags =
    Script


stackLimit : Int
stackLimit =
    100000


printLn : String -> Console -> Console
printLn string console =
    ConsoleString string :: console


printResult : Interpreter -> Console -> Console
printResult interpreter console =
    case interpreter of
        Done ( value, _ ) ->
            Success value :: console

        Panic e ->
            Failure e :: console

        _ ->
            console


traceUi : UiModel -> Console -> Console
traceUi model console =
    UiTrace model :: console


recordSavepoint : Located (Enclojure.Types.Step MyIO) -> Console -> Console
recordSavepoint (Located loc ( ret, thunk )) console =
    ret
        |> Result.map (\val -> Savepoint_ ( Located loc val, thunk ) :: console)
        |> Result.withDefault console


scriptName : Script -> String
scriptName { name } =
    let
        trimmedName =
            String.trim name
    in
    if not <| String.isEmpty trimmedName then
        String.trim name

    else
        "unnamed script"


sleep : Enclojure.Types.Callable MyIO
sleep =
    let
        arity1 val =
            case val of
                Number (Enclojure.Types.Int i) ->
                    Process.sleep (toFloat i)
                        |> Task.map (\_ -> Nil)
                        |> IOTask
                        |> SideEffect
                        |> Ok

                Number (Enclojure.Types.Float i) ->
                    Process.sleep i
                        |> Task.map (\_ -> Nil)
                        |> IOTask
                        |> SideEffect
                        |> Ok

                _ ->
                    Err (Exception "type error: sleep expects one integer argument" [])
    in
    { emptyCallable
        | arity1 = Just (Enclojure.Types.Fixed (Runtime.toFunction arity1))
    }


savepoint : Enclojure.Types.Callable MyIO
savepoint =
    let
        arity1 val =
            Ok (SideEffect (Savepoint val))
    in
    { emptyCallable
        | arity1 = Just (Enclojure.Types.Fixed (Runtime.toFunction arity1))
    }


type alias HttpRequest =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Maybe String
    }


decodeHttpRequest : Value MyIO -> Result Exception HttpRequest
decodeHttpRequest value =
    value
        |> Runtime.tryMap
        |> Result.fromMaybe (Exception "type error: request must be a map" [])
        |> Result.andThen
            (\requestMap ->
                let
                    urlResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Keyword "url")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen Runtime.tryString
                            |> Result.fromMaybe (Exception "type error: :url must be a string" [])

                    headers =
                        requestMap
                            |> Enclojure.ValueMap.get (Keyword "headers")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen (Runtime.tryDictOf Runtime.tryString Runtime.tryString)
                            |> Maybe.map Dict.toList
                            |> Maybe.withDefault []

                    methodResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Keyword "method")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen Runtime.tryKeyword
                            |> Result.fromMaybe (Exception "type error: method must be a keyword" [])

                    bodyResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Keyword "body")
                            |> Maybe.map Located.getValue
                            |> Maybe.withDefault Nil
                            |> (\bodyValue ->
                                    case bodyValue of
                                        String s ->
                                            Just (Just s)

                                        Nil ->
                                            Just Nothing

                                        _ ->
                                            Nothing
                               )
                            |> Result.fromMaybe (Exception "type error: body must be nil or string" [])
                in
                Result.map3
                    (\url method body ->
                        { method = String.toUpper method
                        , headers = headers
                        , url = url
                        , body = body
                        }
                    )
                    urlResult
                    methodResult
                    bodyResult
            )


http : Enclojure.Types.Callable MyIO
http =
    let
        arity1 request env k =
            case decodeHttpRequest request of
                Ok req ->
                    ( Ok ( SideEffect (Http req), env )
                    , Just
                        (Thunk
                            (\v rEnv ->
                                Located.sameAs v ( Ok ( Const <| Located.getValue v, rEnv ), Just (Thunk k) )
                            )
                        )
                    )

                Err exception ->
                    ( Err ( exception, env ), Just (Thunk k) )
    in
    { emptyCallable | arity1 = Just (Enclojure.Types.Fixed arity1) }


ui : Enclojure.Types.Callable MyIO
ui =
    let
        toTextPart (Located _ val) =
            case val of
                String s ->
                    Ok (Plain s)

                Vector v ->
                    case Array.toList v of
                        [ Located _ (Keyword "$"), Located _ (Keyword key) ] ->
                            Ok (TextRef key)

                        _ ->
                            Err (Exception "type error: invalid text formatter" [])

                _ ->
                    Err (Exception "text parts must be plain strings" [])

        toUi (Located _ val) =
            case val of
                Vector v ->
                    case Array.toList v of
                        (Located _ (Keyword "v-stack")) :: cells ->
                            cells
                                |> List.foldl
                                    (\e acc ->
                                        acc
                                            |> Result.andThen
                                                (\l ->
                                                    toUi e
                                                        |> Result.map
                                                            (\retCell ->
                                                                retCell :: l
                                                            )
                                                )
                                    )
                                    (Ok [])
                                |> Result.map (List.reverse >> VStack)

                        (Located _ (Keyword "h-stack")) :: cells ->
                            cells
                                |> List.foldl
                                    (\e acc ->
                                        acc
                                            |> Result.andThen
                                                (\l ->
                                                    toUi e
                                                        |> Result.map
                                                            (\retCell ->
                                                                retCell :: l
                                                            )
                                                )
                                    )
                                    (Ok [])
                                |> Result.map (List.reverse >> HStack)

                        (Located _ (Keyword "text")) :: parts ->
                            parts
                                |> List.foldr (\e a -> toTextPart e |> Result.map2 (\x y -> y :: x) a) (Ok [])
                                |> Result.map Text

                        (Located _ (Keyword "text-input")) :: args ->
                            case args of
                                (Located _ (Keyword key)) :: restArgs ->
                                    let
                                        options =
                                            restArgs
                                                |> List.head
                                                |> Maybe.map Located.getValue
                                                |> Maybe.andThen Runtime.tryMap
                                                |> Maybe.withDefault Enclojure.ValueMap.empty

                                        suggestions =
                                            options
                                                |> Enclojure.ValueMap.get (Keyword "suggestions")
                                                |> Maybe.map Located.getValue
                                                |> Maybe.andThen (Runtime.trySequenceOf Runtime.tryString)
                                                |> Maybe.withDefault []
                                    in
                                    Ok (Input key (TextInput { suggestions = suggestions }))

                                _ ->
                                    Err (Exception "type error: invalid arguments to text-input cell" [])

                        (Located _ (Keyword "button")) :: args ->
                            case args of
                                (Located _ (Keyword key)) :: restArgs ->
                                    let
                                        options =
                                            case restArgs of
                                                (Located _ (Map m)) :: _ ->
                                                    m

                                                _ ->
                                                    Enclojure.ValueMap.empty

                                        title =
                                            Enclojure.ValueMap.get (Keyword "title") options
                                                |> Maybe.map Located.getValue
                                                |> Maybe.andThen Runtime.tryString
                                                |> Maybe.withDefault key
                                    in
                                    Ok (Input key (Button { title = title }))

                                _ ->
                                    Err (Exception "type error: invalid arguments to button cell" [])

                        (Located _ (Keyword "download")) :: args ->
                            case args of
                                (Located _ (Keyword key)) :: (Located _ (String content)) :: restArgs ->
                                    let
                                        options =
                                            case restArgs of
                                                (Located _ (Map m)) :: _ ->
                                                    m

                                                _ ->
                                                    Enclojure.ValueMap.empty

                                        contentType =
                                            Enclojure.ValueMap.get (Keyword "content-type") options
                                                |> Maybe.map Located.getValue
                                                |> Maybe.andThen Runtime.tryString
                                                |> Maybe.withDefault "text/plain"

                                        name =
                                            Enclojure.ValueMap.get (Keyword "name") options
                                                |> Maybe.map Located.getValue
                                                |> Maybe.andThen Runtime.tryString
                                                |> Maybe.withDefault "download.txt"
                                    in
                                    Ok
                                        (Input key
                                            (Download
                                                { name = name
                                                , content = content
                                                , contentType = contentType
                                                }
                                            )
                                        )

                                _ ->
                                    Err (Exception "type error: invalid arguments to download cell" [])

                        (Located _ (Keyword cellType)) :: _ ->
                            Err (Exception ("type error: " ++ cellType ++ " is not a supported cell type") [])

                        _ :: _ ->
                            Err (Exception "type error: cell type must be a keyword" [])

                        [] ->
                            Err (Exception "type error: empty vector is not a valid cell" [])

                _ ->
                    Err (Exception "cell must be a vector" [])

        arity1 val =
            toUi (Located.unknown val)
                |> Result.map
                    (\cell ->
                        SideEffect <| ShowUI { cell = cell, watchFn = Nil, state = Enclojure.ValueMap.empty }
                    )

        arity2 ( uiVal, defaultsMap ) =
            arity3 ( uiVal, Nil, defaultsMap )

        arity3 ( uiVal, watchFn, defaultsMap ) =
            case defaultsMap of
                Map m ->
                    toUi (Located.unknown uiVal)
                        |> Result.map (\cell -> SideEffect <| ShowUI { cell = cell, watchFn = watchFn, state = m })

                _ ->
                    Err (Exception ("type error: expected a map of defaults, got " ++ Runtime.inspect defaultsMap) [])
    in
    { emptyCallable
        | arity1 = Just (Enclojure.Types.Fixed (Runtime.toFunction arity1))
        , arity2 = Just (Enclojure.Types.Fixed (Runtime.toFunction arity2))
        , arity3 = Just (Enclojure.Types.Fixed (Runtime.toFunction arity3))
    }


type MyIO
    = IOTask (Task.Task Exception (Value MyIO))
    | ShowUI (UI MyIO)
    | Savepoint (Value MyIO)
    | Http HttpRequest


type Message
    = FuzzySelectMessage String LanternUi.FuzzySelect.Message
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell String
    | UpdateScripts (Result Lantern.Error (List Script))
    | HandleIO (Located (Enclojure.Types.Step MyIO))
    | Rewind (Located (Enclojure.Types.Step MyIO))
    | InspectValue (Value MyIO)
    | CreateScript
    | ScriptCreated (Result Lantern.Error Lantern.Query.WriterResult)
    | SaveScript
    | ScriptSaved (Result Lantern.Error Lantern.Query.WriterResult)
    | NewScript
    | DeleteScript Script
    | EditScript Script
    | UpdateName String
    | UpdateCode String
    | UpdateRepl String
    | UpdateBrowserQuery String
    | Eval String
    | DownloadFile String String String
    | NoOp


init : Maybe Flags -> ( Model, Cmd (Lantern.App.Message Message) )
init flags =
    flags
        |> Maybe.map
            (\script ->
                updateRunner
                    (Runner
                        { interpreter = Stopped
                        , script = script
                        , console = []
                        , repl = ""
                        }
                    )
                    runScript
            )
        |> Maybe.withDefault ( Browser { scripts = [], query = "" }, Cmd.none )


unsavedScript : { id : Maybe Int, code : String, name : String, input : String }
unsavedScript =
    { id = Nothing, code = "", name = "", input = "" }


type alias UI io =
    { state : Enclojure.Types.ValueMap io
    , cell : Cell
    , watchFn : Value io
    }


type alias UiModel =
    { fuzzySelects : Dict String FuzzySelect, enclojureUi : UI MyIO }


type Interpreter
    = Stopped
    | ShowingUI UiModel ( Env MyIO, Maybe (Thunk MyIO) )
    | Running
    | Done ( Value MyIO, Env MyIO )
    | Panic ( Located Exception, Env MyIO )


responseToValue : Lantern.Http.Response -> Value MyIO
responseToValue response =
    Map <|
        Enclojure.ValueMap.fromList
            [ ( Keyword "status", Located.unknown <| Number (Enclojure.Types.Int response.status) )
            , ( Keyword "headers"
              , Located.unknown <|
                    Map
                        (response.headers
                            |> List.map (\( k, v ) -> ( String k, Located.unknown (String v) ))
                            |> Enclojure.ValueMap.fromList
                        )
              )
            , ( Keyword "body"
              , response.body |> Maybe.map String |> Maybe.withDefault Nil |> Located.unknown
              )
            ]


trampoline :
    Located (Enclojure.Types.Step MyIO)
    -> Int
    -> ( Interpreter, Cmd (Lantern.App.Message Message) )
trampoline (Located loc ( result, thunk )) maxdepth =
    case result of
        Ok ( io, env ) ->
            if maxdepth <= 0 then
                ( Panic ( Located.unknown (Exception "Stack level too deep" []), env ), Cmd.none )

            else
                case io of
                    Const v ->
                        case thunk of
                            Just (Thunk continuation) ->
                                trampoline (continuation (Located loc v) env) (maxdepth - 1)

                            Nothing ->
                                ( Done ( v, env ), Cmd.none )

                    SideEffect se ->
                        case se of
                            Http request ->
                                ( Running
                                , Lantern.httpRequest
                                    { method = request.method
                                    , headers = request.headers
                                    , url = request.url
                                    , body = request.body
                                    , expect =
                                        \response ->
                                            ( Ok ( Const (responseToValue response), env ), thunk )
                                                |> Located loc
                                                |> HandleIO
                                    }
                                    |> Lantern.App.call
                                )

                            IOTask task ->
                                ( Running
                                , task
                                    |> Task.attempt
                                        (\r ->
                                            ( r
                                                |> Result.mapError (\e -> ( e, env ))
                                                |> Result.map (\v -> ( Const v, env ))
                                            , thunk
                                            )
                                                |> Located loc
                                                |> HandleIO
                                                |> Lantern.App.Message
                                        )
                                )

                            ShowUI uiState ->
                                ( ShowingUI { enclojureUi = uiState, fuzzySelects = Dict.empty } ( env, thunk )
                                , Cmd.none
                                )

                            Savepoint v ->
                                ( Running
                                , ( Ok ( Const v, env ), thunk )
                                    |> Located loc
                                    |> HandleIO
                                    |> Lantern.App.Message
                                    |> Task.succeed
                                    |> Task.perform identity
                                )

        Err e ->
            ( Panic (e |> Tuple.mapFirst (Located loc)), Cmd.none )


updateBrowser :
    Model
    -> (BrowserModel -> ( BrowserModel, Cmd (Lantern.App.Message Message) ))
    -> ( Model, Cmd (Lantern.App.Message Message) )
updateBrowser model updateFn =
    case model of
        Browser m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Browser newM, cmd )

        _ ->
            ( model, Cmd.none )


updateEditor :
    Model
    -> (EditorModel -> ( EditorModel, Cmd (Lantern.App.Message Message) ))
    -> ( Model, Cmd (Lantern.App.Message Message) )
updateEditor model updateFn =
    case model of
        Editor m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Editor newM, cmd )

        _ ->
            ( model, Cmd.none )


updateRunner :
    Model
    -> (EditorModel -> ( EditorModel, Cmd (Lantern.App.Message Message) ))
    -> ( Model, Cmd (Lantern.App.Message Message) )
updateRunner model updateFn =
    case model of
        Editor m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Editor newM, cmd )

        Runner m ->
            let
                ( newM, cmd ) =
                    updateFn m
            in
            ( Runner newM, cmd )

        _ ->
            ( model, Cmd.none )


runWatchFn :
    Env MyIO
    -> Value MyIO
    -> Enclojure.Types.ValueMap MyIO
    -> Result (Located Exception) (Enclojure.Types.ValueMap MyIO)
runWatchFn env watchFn stateMap =
    case watchFn of
        Nil ->
            Ok stateMap

        _ ->
            let
                ( interpreter, _ ) =
                    trampoline
                        (Runtime.apply (Located.unknown watchFn)
                            (Located.unknown (List [ Located.unknown (Map stateMap) ]))
                            env
                            Enclojure.terminate
                        )
                        stackLimit
            in
            case interpreter of
                Done ( val, _ ) ->
                    val
                        |> Runtime.tryMap
                        |> Result.fromMaybe (Located.unknown (Exception "type error: watch returned a non-map" []))

                Panic ( err, _ ) ->
                    Err err

                _ ->
                    Err (Located.unknown (Exception "runtime error: watch fn tried to run a side effect" []))


defaultEnv : Env MyIO
defaultEnv =
    Enclojure.defaultEnv
        |> Runtime.setGlobalEnv "http/request"
            (Fn (Just "http/request") (Runtime.toThunk http))
        |> Runtime.setGlobalEnv "sleep"
            (Fn (Just "sleep") (Runtime.toThunk sleep))
        |> Runtime.setGlobalEnv "ui"
            (Fn (Just "ui") (Runtime.toThunk ui))
        |> Runtime.setGlobalEnv "<o>"
            (Fn (Just "<o>") (Runtime.toThunk savepoint))


runScript : EditorModel -> ( EditorModel, Cmd (Lantern.App.Message Message) )
runScript model =
    let
        ( interpreter, retMsg ) =
            trampoline (Enclojure.eval defaultEnv model.script.code) stackLimit
    in
    ( { model
        | interpreter = interpreter
        , console =
            model.console
                |> printLn ("Starting " ++ scriptName model.script)
                |> printResult interpreter
      }
    , retMsg
    )


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg appModel =
    case msg of
        DownloadFile filename contentType content ->
            ( appModel, File.Download.string filename contentType content )

        DeleteScript script ->
            updateBrowser appModel
                (\model ->
                    script.id
                        |> Maybe.map
                            (\id ->
                                Lantern.Query.withArguments
                                    "DELETE FROM scripts WHERE id=$id"
                                    [ ( "$id", id |> Lantern.Query.Int )
                                    ]
                            )
                        |> Maybe.map (\query -> ( model, Lantern.writerQuery query ScriptSaved |> Lantern.App.call ))
                        |> Maybe.withDefault ( model, Cmd.none )
                )

        FuzzySelectMessage id selectMsg ->
            updateRunner appModel
                (\model ->
                    case model.interpreter of
                        ShowingUI ({ fuzzySelects } as uiState) args ->
                            let
                                updatedFuzzySelects =
                                    Dict.update id
                                        (Maybe.withDefault LanternUi.FuzzySelect.hidden
                                            >> LanternUi.FuzzySelect.update selectMsg
                                            >> Just
                                        )
                                        fuzzySelects
                            in
                            ( { model | interpreter = ShowingUI { uiState | fuzzySelects = updatedFuzzySelects } args }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )
                )

        UpdateCode code ->
            updateEditor appModel
                (\model ->
                    let
                        script =
                            model.script

                        editor =
                            { script | code = code }
                    in
                    ( { model | script = editor }, Cmd.none )
                )

        UpdateRepl code ->
            updateEditor appModel
                (\model ->
                    ( { model | repl = code }, Cmd.none )
                )

        Eval code ->
            updateEditor appModel
                (\model ->
                    let
                        mEnv =
                            case model.interpreter of
                                Done ( _, env ) ->
                                    Just env

                                Panic ( _, env ) ->
                                    Just env

                                _ ->
                                    Nothing
                    in
                    mEnv
                        |> Maybe.map
                            (\env ->
                                let
                                    ( interpreter, retMsg ) =
                                        trampoline (Enclojure.eval env code) stackLimit
                                in
                                ( { model
                                    | interpreter = interpreter
                                    , console =
                                        model.console
                                            |> printLn "Evaluating from REPL"
                                            |> printResult interpreter
                                    , repl = ""
                                  }
                                , retMsg
                                )
                            )
                        |> Maybe.withDefault
                            ( { model
                                | console =
                                    model.console
                                        |> printLn "Can't eval from REPL at this time"
                              }
                            , Cmd.none
                            )
                )

        UpdateName name ->
            updateEditor appModel
                (\model ->
                    let
                        script =
                            model.script

                        newScript =
                            { script | name = name }
                    in
                    ( { model | script = newScript }, Cmd.none )
                )

        Run ->
            updateEditor appModel runScript

        Stop ->
            updateEditor appModel
                (\model ->
                    ( { model
                        | interpreter = Panic ( Located.unknown (Exception "Terminated" []), Runtime.emptyEnv )
                      }
                    , Cmd.none
                    )
                )

        HandleIO ret ->
            updateRunner appModel
                (\model ->
                    let
                        ( interpreter, retMsg ) =
                            trampoline ret stackLimit
                    in
                    ( { model
                        | interpreter = interpreter
                        , console =
                            model.console
                                |> recordSavepoint ret
                                |> printResult interpreter
                      }
                    , retMsg
                    )
                )

        Rewind ret ->
            let
                ( newAppModel, _ ) =
                    updateEditor appModel (\model -> ( { model | interpreter = Running }, Cmd.none ))
            in
            update (HandleIO ret) newAppModel

        UpdateInputRequest name inputType v ->
            updateRunner
                appModel
                (\model ->
                    case model.interpreter of
                        ShowingUI ({ enclojureUi } as uiState) ( env, thunk ) ->
                            case inputType of
                                Button _ ->
                                    let
                                        exitCode =
                                            Located.unknown <| Keyword name

                                        state =
                                            Located.unknown <| Map enclojureUi.state

                                        console =
                                            model.console
                                                |> traceUi uiState
                                                |> printResult model.interpreter
                                    in
                                    ( { model | interpreter = Running, console = console }
                                    , ( Ok ( Const (List [ exitCode, state ]), env ), thunk )
                                        |> Located.unknown
                                        |> HandleIO
                                        |> Lantern.App.Message
                                        |> Task.succeed
                                        |> Task.perform identity
                                    )

                                _ ->
                                    let
                                        updatedState =
                                            Enclojure.ValueMap.insert (Keyword name)
                                                (Located.unknown (String v))
                                                uiState.enclojureUi.state
                                                |> runWatchFn env uiState.enclojureUi.watchFn

                                        ( interpreter, console ) =
                                            case updatedState of
                                                Ok st ->
                                                    let
                                                        updatedUi =
                                                            { enclojureUi | state = st }
                                                    in
                                                    ( ShowingUI { uiState | enclojureUi = updatedUi } ( env, thunk )
                                                    , model.console
                                                    )

                                                Err ex ->
                                                    let
                                                        i =
                                                            Panic ( ex, env )
                                                    in
                                                    ( i, printResult i model.console )

                                        updatedModel =
                                            { model | interpreter = interpreter, console = console }
                                    in
                                    ( updatedModel, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
                )

        UpdateScripts result ->
            updateBrowser appModel
                (\model ->
                    case result of
                        Ok scripts ->
                            ( { model | scripts = scripts }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )
                )

        SaveScript ->
            updateEditor appModel
                (\model ->
                    let
                        cmd =
                            model.script.id
                                |> Maybe.map
                                    (\id ->
                                        Lantern.Query.withArguments
                                            "UPDATE scripts SET name=$name, code=$code, input=$input, updated_at=datetime('now') WHERE id=$id"
                                            [ ( "$name", model.script.name |> Lantern.Query.String )
                                            , ( "$code", model.script.code |> Lantern.Query.String )
                                            , ( "$input", model.script.input |> Lantern.Query.String )
                                            , ( "$id", id |> Lantern.Query.Int )
                                            ]
                                    )
                                |> Maybe.map (\query -> Lantern.writerQuery query ScriptSaved |> Lantern.App.call)
                                |> Maybe.withDefault Cmd.none
                    in
                    ( model, cmd )
                )

        NewScript ->
            ( Editor
                { interpreter = Stopped
                , script = unsavedScript
                , console = []
                , repl = ""
                }
            , Cmd.none
            )

        ScriptSaved _ ->
            ( appModel, Cmd.none )

        CreateScript ->
            updateEditor appModel
                (\model ->
                    let
                        query =
                            Lantern.Query.withArguments
                                "INSERT INTO scripts (name, code, input, created_at, updated_at) VALUES ($name, $code, $input, datetime('now'), datetime('now'))"
                                [ ( "$name", model.script.name |> Lantern.Query.String )
                                , ( "$code", model.script.code |> Lantern.Query.String )
                                , ( "$input", model.script.input |> Lantern.Query.String )
                                ]
                    in
                    ( model, Lantern.writerQuery query ScriptCreated |> Lantern.App.call )
                )

        ScriptCreated result ->
            updateEditor appModel
                (\model ->
                    result
                        |> Result.map
                            (\writerResult ->
                                let
                                    editor =
                                        model.script

                                    newEditor =
                                        { editor | id = Just writerResult.lastInsertRowId }
                                in
                                ( { model | script = newEditor }, Cmd.none )
                            )
                        |> Result.withDefault ( model, Cmd.none )
                )

        EditScript script ->
            ( Editor
                { interpreter = Stopped
                , script = script
                , console = []
                , repl = ""
                }
            , Cmd.none
            )

        -- Beware: the Apps namespace hijacks this message and launches the inspector
        -- would be nice to make this more explicit but it'll take a lot of refactoring
        InspectValue _ ->
            ( appModel, Cmd.none )

        NoOp ->
            ( appModel, Cmd.none )

        UpdateBrowserQuery query ->
            updateBrowser appModel
                (\model ->
                    ( { model | query = query }
                    , Cmd.none
                    )
                )


renderUI : Context -> UiModel -> Element (Lantern.App.Message Message)
renderUI context uiModel =
    let
        { cell, state, watchFn } =
            uiModel.enclojureUi

        { fuzzySelects } =
            uiModel
    in
    case cell of
        VStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { cell = c, watchFn = watchFn, state = state } })
                |> Element.column [ Element.width Element.fill, Element.alignTop, Element.spacing 10 ]

        HStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { cell = c, watchFn = watchFn, state = state } })
                |> Element.row [ Element.width Element.fill ]

        Input key inputType ->
            let
                val =
                    Enclojure.ValueMap.get (Keyword key) state
                        |> Maybe.map (Located.getValue >> Runtime.toString)
                        |> Maybe.withDefault ""
            in
            case inputType of
                TextInput opts ->
                    if List.isEmpty opts.suggestions then
                        LanternUi.Input.multiline
                            context.theme
                            []
                            { onChange = UpdateInputRequest key inputType >> Lantern.App.Message
                            , placeholder = Nothing
                            , label = Element.Input.labelHidden key
                            , text = val
                            , spellcheck = False
                            }

                    else
                        LanternUi.FuzzySelect.fuzzySelect
                            context.theme
                            { label = Element.Input.labelHidden ""
                            , onQueryChange = UpdateInputRequest key inputType >> Lantern.App.Message
                            , onInternalMessage = FuzzySelectMessage key >> Lantern.App.Message
                            , onOptionSelect = UpdateInputRequest key inputType >> Lantern.App.Message
                            , options = List.map2 Tuple.pair opts.suggestions opts.suggestions
                            , placeholder = Nothing
                            , query = val
                            , state = Dict.get key fuzzySelects |> Maybe.withDefault LanternUi.FuzzySelect.hidden
                            , id = Nothing
                            }

                MaskedTextInput ->
                    LanternUi.Input.password
                        context.theme
                        []
                        { onChange = UpdateInputRequest key inputType >> Lantern.App.Message
                        , placeholder = Nothing
                        , label = Element.Input.labelHidden ""
                        , text = val
                        , show = False
                        }

                Button { title } ->
                    LanternUi.Input.button
                        context.theme
                        []
                        { onPress = Just (UpdateInputRequest key inputType "" |> Lantern.App.Message)
                        , label = Element.text title
                        }

                Download { name, contentType, content } ->
                    LanternUi.Input.button
                        context.theme
                        []
                        { onPress = Just (DownloadFile name contentType content |> Lantern.App.Message)
                        , label = Element.text "Download"
                        }

        Text textCells ->
            textCells
                |> List.map
                    (\c ->
                        case c of
                            Plain t ->
                                Element.text t

                            TextRef key ->
                                let
                                    val =
                                        Enclojure.ValueMap.get (Keyword key) state
                                            |> Maybe.map (Located.getValue >> Runtime.toString)
                                            |> Maybe.withDefault ""
                                in
                                Element.text val
                    )
                |> Element.paragraph []


activeUi : Interpreter -> Maybe UiModel
activeUi interpreter =
    case interpreter of
        ShowingUI model _ ->
            Just model

        _ ->
            Nothing


type alias ConsoleOptions =
    { devMode : Bool }


isDevModeOnlyEntry : ConsoleEntry -> Bool
isDevModeOnlyEntry entry =
    case entry of
        Savepoint_ _ ->
            True

        _ ->
            False


viewConsole : Context -> Interpreter -> Console -> ConsoleOptions -> Element (Lantern.App.Message Message)
viewConsole context interpreter console options =
    activeUi interpreter
        |> Maybe.map (\uiState -> UiTrace uiState :: console)
        |> Maybe.withDefault console
        |> List.filter (\entry -> options.devMode || not (isDevModeOnlyEntry entry))
        |> List.take 20
        |> List.indexedMap
            (\i entry ->
                let
                    valueRow val =
                        Element.paragraph
                            [ Element.width Element.fill ]
                            [ Element.text (Runtime.inspect val)
                            , LanternUi.Input.button context.theme
                                []
                                { onPress = Just (Lantern.App.Message <| InspectValue val)
                                , label = Element.text "Inspect"
                                }
                            ]
                in
                Element.el
                    [ Element.width Element.fill
                    , if i == 0 then
                        Element.alpha 1.0

                      else
                        Element.alpha 0.6
                    ]
                    (case entry of
                        ConsoleString s ->
                            Element.paragraph [ Element.width Element.fill ] [ Element.text s ]

                        Savepoint_ ( Located loc ( io, env ), thunk ) ->
                            let
                                newEnv =
                                    case interpreter of
                                        Done ( _, cEnv ) ->
                                            { cEnv | local = env.local }

                                        Panic ( _, cEnv ) ->
                                            { cEnv | local = env.local }

                                        _ ->
                                            env

                                rewindButton =
                                    LanternUi.Input.button context.theme
                                        []
                                        { onPress = Just (Lantern.App.Message <| Rewind (Located loc ( Ok ( io, newEnv ), thunk )))
                                        , label = Element.text "Rewind"
                                        }
                            in
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.paragraph [] [ Element.text "Savepoint", rewindButton ]
                                , case io of
                                    Const v ->
                                        valueRow v

                                    _ ->
                                        Element.none
                                ]

                        Success val ->
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.paragraph [ Element.width Element.fill ] [ Element.text "Done" ]
                                , valueRow val
                                ]

                        Failure ( e, _ ) ->
                            Element.text <| Runtime.inspectLocated (Located.map Throwable e)

                        UiTrace uiState ->
                            Element.column
                                [ Element.width Element.fill ]
                                [ renderUI context uiState ]
                    )
            )
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]


viewEditor : Context -> EditorModel -> Element (Lantern.App.Message Message)
viewEditor context model =
    let
        runButtonTitle =
            case model.interpreter of
                Stopped ->
                    "Start"

                _ ->
                    "Restart"

        runButton =
            LanternUi.Input.button context.theme
                []
                { label = Element.text runButtonTitle, onPress = Just <| Lantern.App.Message Run }

        evalButton =
            LanternUi.Input.button context.theme
                []
                { label = Element.text "Eval", onPress = Just <| Lantern.App.Message (Eval model.script.code) }

        saveButton =
            if model.script.id == Nothing then
                LanternUi.Input.button context.theme
                    []
                    { label = Element.text "Save", onPress = Just <| Lantern.App.Message CreateScript }

            else
                LanternUi.Input.button context.theme
                    []
                    { label = Element.text "Save", onPress = Just <| Lantern.App.Message SaveScript }

        toolbar =
            Element.row
                [ Element.width Element.fill, Element.spacing 10 ]
                [ saveButton, runButton, evalButton ]

        scriptEditor =
            Element.column
                [ Element.width Element.fill, Element.spacing 20 ]
                [ LanternUi.Input.text context.theme
                    [ Element.width Element.fill ]
                    { onChange = UpdateName >> Lantern.App.Message
                    , text = model.script.name
                    , placeholder = Nothing
                    , label = Element.Input.labelAbove [] (Element.text "Script name")
                    }
                , LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    ]
                    { onChange = UpdateCode >> Lantern.App.Message
                    , value = model.script.code
                    , language = LanternUi.Input.Enclojure
                    , label = Just <| Element.text "Code"
                    }
                ]

        handleKeyPress event =
            case ( event.keyCode, event.metaKey, event.altKey ) of
                ( Keyboard.Key.Enter, True, False ) ->
                    ( Eval model.repl |> Lantern.App.Message, True )

                _ ->
                    ( NoOp |> Lantern.App.Message, False )

        repl =
            Element.column [ Element.width Element.fill, Element.spacing 20 ]
                [ LanternUi.Input.code context.theme
                    [ Element.width Element.fill
                    , Json.Decode.map handleKeyPress Keyboard.Event.decodeKeyboardEvent
                        |> Html.Events.preventDefaultOn "keydown"
                        |> Element.htmlAttribute
                    ]
                    { onChange = UpdateRepl >> Lantern.App.Message
                    , value = model.repl
                    , language = LanternUi.Input.Enclojure
                    , label = Just <| Element.text "REPL"
                    }
                , Element.paragraph [] [ Element.text "Console" ]
                , viewConsole context model.interpreter model.console { devMode = True }
                ]
    in
    LanternUi.columnLayout
        context.theme
        [ Element.width Element.fill, Element.spacing 20 ]
        [ toolbar
        , Element.column [ Element.width (Element.fillPortion 5), Element.spacing 20 ] [ scriptEditor, repl ]
        ]


viewBrowser : Context -> BrowserModel -> Element (Lantern.App.Message Message)
viewBrowser context model =
    let
        newScriptButton =
            Element.el
                [ Element.width Element.fill ]
                (LanternUi.Input.button context.theme
                    []
                    { label = Element.text "New script", onPress = Just <| Lantern.App.Message NewScript }
                )

        search =
            LanternUi.Input.text
                context.theme
                []
                { text = model.query
                , onChange = UpdateBrowserQuery >> Lantern.App.Message
                , placeholder = Just (Element.Input.placeholder [] (Element.text "Filter scripts"))
                , label = Element.Input.labelHidden "Filter scripts"
                }

        toolbar =
            Element.row
                [ Element.spacing 10 ]
                [ search
                , newScriptButton
                ]

        filteredScripts =
            model.scripts
                |> List.filter
                    (\s ->
                        String.isEmpty model.query
                            || String.contains (String.toLower model.query) (String.toLower s.name)
                    )

        scriptsList =
            filteredScripts
                |> List.map
                    (\script ->
                        Element.row
                            [ Element.width Element.fill
                            , Element.spacing 20
                            ]
                            [ Element.Input.button
                                [ Element.mouseOver [ Element.Background.color context.theme.bgHighlight ] ]
                                { onPress = Just (Lantern.App.Message <| EditScript script)
                                , label = Element.text (scriptName script)
                                }
                            , LanternUi.Input.button context.theme
                                []
                                { onPress = Just (Lantern.App.Message <| DeleteScript script)
                                , label = Element.text "Delete"
                                }
                            ]
                    )
                |> Element.column [ Element.width (Element.fillPortion 1), Element.alignTop, Element.spacing 10 ]
    in
    LanternUi.columnLayout
        context.theme
        []
        [ toolbar
        , scriptsList
        ]


viewRunner : Context -> EditorModel -> Element (Lantern.App.Message Message)
viewRunner context model =
    viewConsole context model.interpreter model.console { devMode = False }


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    case model of
        Browser browser ->
            viewBrowser context browser

        Editor editor ->
            viewEditor context editor

        Runner runner ->
            viewRunner context runner


type alias Script =
    { id : Maybe Int
    , name : String
    , code : String
    , input : String
    }


scriptDecoder : Json.Decode.Decoder Script
scriptDecoder =
    Json.Decode.map4
        Script
        (Json.Decode.map Just (Json.Decode.field "id" Json.Decode.int))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "input" Json.Decode.string)


scriptsQuery : (Result Lantern.Error (List Script) -> msg) -> LiveQuery msg
scriptsQuery =
    Lantern.LiveQuery.prepare
        ( Lantern.Query.Query "SELECT id, input, name, code FROM scripts ORDER BY name" Dict.empty, scriptDecoder )


liveQueries : Model -> List (LiveQuery Message)
liveQueries model =
    case model of
        Browser _ ->
            [ scriptsQuery UpdateScripts ]

        Editor _ ->
            []

        Runner _ ->
            []


lanternApp : Lantern.App.App Context Flags Model Message
lanternApp =
    Lantern.App.app
        { name = "Scripts"
        , init = init
        , view = view
        , update = update
        , liveQueries = Just liveQueries
        , subscriptions = always Sub.none
        }
