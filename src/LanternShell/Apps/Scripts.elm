module LanternShell.Apps.Scripts exposing (Flags(..), Message(..), Model, MyIO, Script, ScriptId, lanternApp, scriptsQuery)

import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Input
import Enclojure exposing (Env, EvalResult, Exception, Step)
import Enclojure.Callable as Callable exposing (Callable)
import Enclojure.Json
import Enclojure.Located as Located
import Enclojure.Runtime as Runtime
import Enclojure.Value as Value exposing (Value)
import Enclojure.ValueKeyMap exposing (ValueKeyMap)
import Enclojure.ValueMap exposing (ValueMap)
import File.Download
import Html.Events
import Json.Decode
import Json.Encode
import Keyboard.Event
import Keyboard.Key exposing (Key(..))
import Lantern
import Lantern.App
import Lantern.Errors
import Lantern.Http
import Lantern.LiveQuery exposing (LiveQuery)
import Lantern.Query
import LanternUi
import LanternUi.FuzzySelect exposing (FuzzySelect)
import LanternUi.Input
import LanternUi.Persistent exposing (Persistent)
import LanternUi.Theme
import Markdown
import Process
import Task


type alias Context =
    { theme : LanternUi.Theme.Theme }


type TextFormat
    = Plain String
    | TextRef InputKey
    | Markdown String


type alias ScriptId =
    Int


type InputCell
    = TextInput { suggestions : List String }
    | MaskedTextInput
    | Button { title : String }
    | Checkbox { label : String }
    | Download { name : String, contentType : String, content : String }


type alias InputKey =
    Value MyIO


type Cell
    = Input InputKey InputCell
    | Text (List TextFormat)
    | VStack (List Cell)
    | HStack (List Cell)
    | Image { width : Maybe Int } String
    | Empty


type ConsoleEntry
    = ConsoleString String
    | UiTrace UiModel
    | Savepoint_ (Step MyIO)
    | Success (Value MyIO)
    | Failure ( Exception, Env MyIO )


type alias Console =
    List ConsoleEntry


type alias BrowserModel =
    { scripts : List ( ScriptId, Script )
    , query : String
    }


type alias EditorModel =
    { interpreter : Interpreter
    , script : Script
    , serverScript : Maybe Script
    , console : Console
    , repl : String
    }


type Model
    = Browser BrowserModel
    | Editor (Persistent ScriptId EditorModel)
    | Runner (Persistent ScriptId EditorModel)


type Flags
    = BrowserMode
    | NewScriptMode
    | EditScriptMode Int
    | RunnerMode Int


flagsDecoder : Json.Decode.Decoder (Maybe Flags)
flagsDecoder =
    Json.Decode.map2
        (\flagType mId ->
            case ( flagType, mId ) of
                ( "b", Nothing ) ->
                    Just BrowserMode

                ( "e", Just id ) ->
                    Just <| EditScriptMode id

                ( "n", Nothing ) ->
                    Just <| NewScriptMode

                ( "r", Just id ) ->
                    Just <| RunnerMode id

                _ ->
                    Nothing
        )
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.index 1 Json.Decode.int))


encodeFlags : Model -> Maybe Json.Encode.Value
encodeFlags model =
    case model of
        Browser _ ->
            Just <| Json.Encode.list identity [ Json.Encode.string "b" ]

        Editor (LanternUi.Persistent.Loaded id _) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "e", Json.Encode.int id ]

        Editor (LanternUi.Persistent.Loading id) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "e", Json.Encode.int id ]

        Editor (LanternUi.Persistent.New _) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "n" ]

        Runner (LanternUi.Persistent.Loaded id _) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "r", Json.Encode.int id ]

        Runner (LanternUi.Persistent.Loading id) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "r", Json.Encode.int id ]

        Runner (LanternUi.Persistent.New _) ->
            Just <| Json.Encode.list identity [ Json.Encode.string "n" ]


stackLimit : Int
stackLimit =
    100000


emptyEditor : EditorModel
emptyEditor =
    { interpreter = Stopped
    , script = emptyScript
    , serverScript = Nothing
    , console = []
    , repl = ""
    }


printLn : String -> Console -> Console
printLn string console =
    ConsoleString string :: console


printResult : Interpreter -> Console -> Console
printResult interpreter console =
    case interpreter of
        Done value _ ->
            Success value :: console

        Panic e env ->
            Failure ( e, env ) :: console

        _ ->
            console


traceUi : UiModel -> Console -> Console
traceUi model console =
    UiTrace model :: console


recordSavepoint : Step MyIO -> Console -> Console
recordSavepoint step console =
    Savepoint_ step :: console


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


sleep : Callable MyIO
sleep =
    let
        arity1 val =
            val
                |> Value.tryOneOf
                    [ Value.tryInt >> Maybe.map toFloat
                    , Value.tryFloat
                    ]
                |> Result.fromMaybe (Value.exception "type error: sleep expects one numeric argument")
                |> Result.andThen
                    (Process.sleep
                        >> Task.map (\_ -> Value.nil)
                        >> IOTask
                        >> Runtime.sideEffect
                        >> Ok
                    )
    in
    Callable.new
        |> Callable.setArity1 (Callable.fixedArity (Value.symbol "ms") arity1)


savepoint : Callable MyIO
savepoint =
    let
        arity1 val =
            Ok (Runtime.sideEffect (Savepoint val))
    in
    Callable.new
        |> Callable.setArity1 (Callable.fixedArity (Value.symbol "x") arity1)


decodeHttpRequest : Value MyIO -> Result Exception Lantern.Http.RequestPayload
decodeHttpRequest value =
    value
        |> Value.tryMap
        |> Result.fromMaybe (Value.exception "type error: request must be a map")
        |> Result.andThen
            (\requestMap ->
                let
                    urlResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Value.keyword "url")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen Value.tryString
                            |> Result.fromMaybe (Value.exception "type error: :url must be a string")

                    headers =
                        requestMap
                            |> Enclojure.ValueMap.get (Value.keyword "headers")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen (Value.tryDictOf Value.tryString Value.tryString)
                            |> Maybe.map Dict.toList
                            |> Maybe.withDefault []

                    methodResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Value.keyword "method")
                            |> Maybe.map Located.getValue
                            |> Maybe.andThen Value.tryKeyword
                            |> Result.fromMaybe (Value.exception "type error: method must be a keyword")

                    bodyResult =
                        requestMap
                            |> Enclojure.ValueMap.get (Value.keyword "body")
                            |> Maybe.map Located.getValue
                            |> Maybe.withDefault Value.nil
                            |> (\bodyValue ->
                                    bodyValue
                                        |> Value.tryOneOf
                                            [ Value.tryString >> Maybe.map Just
                                            , Value.tryNil >> Maybe.map (always Nothing)
                                            ]
                               )
                            |> Result.fromMaybe (Value.exception "type error: body must be nil or string")
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


http : Callable MyIO
http =
    let
        arity1 request =
            case decodeHttpRequest request of
                Ok req ->
                    Lantern.httpRequest
                        { method = req.method
                        , headers = req.headers
                        , url = req.url
                        , body = req.body
                        , expect = responseToValue >> Ok
                        }
                        |> LanternTask
                        |> Runtime.sideEffect
                        |> Ok

                Err exception ->
                    Err exception
    in
    Callable.new
        |> Callable.setArity1 (Callable.fixedArity (Value.symbol "req") arity1)


echo : Callable MyIO
echo =
    Callable.new
        |> Callable.setArity1
            (Callable.fixedArity
                (Value.symbol "text")
                (\val ->
                    val
                        |> Value.tryString
                        |> Result.fromMaybe (Value.exception "not a string")
                        |> Result.map
                            ((\text -> Lantern.echo text (Value.string >> Ok))
                                >> LanternTask
                                >> Runtime.sideEffect
                            )
                )
            )


readerQuery : Callable MyIO
readerQuery =
    Callable.new
        |> Callable.setArity1
            (Callable.fixedArity
                (Value.symbol "query")
                (\val ->
                    val
                        |> Value.tryString
                        |> Result.fromMaybe (Value.exception "not a string")
                        |> Result.map
                            (\query ->
                                Lantern.readerQuery
                                    (Lantern.Query.withNoArguments query)
                                    Enclojure.Json.decodeValue
                                    (Result.mapError (Lantern.Errors.toString >> Value.exception)
                                        >> Result.map Value.vectorFromList
                                    )
                                    |> LanternTask
                                    |> Runtime.sideEffect
                            )
                )
            )
        |> Callable.setArity2
            (Callable.fixedArity
                ( Value.symbol "query"
                , Value.symbol "args"
                )
                (\( queryVal, argsVal ) ->
                    Result.map2
                        (\query args ->
                            Lantern.readerQuery
                                (Lantern.Query.withArguments query (Dict.toList args))
                                Enclojure.Json.decodeValue
                                (Result.mapError (Lantern.Errors.toString >> Value.exception)
                                    >> Result.map Value.vectorFromList
                                )
                                |> LanternTask
                                |> Runtime.sideEffect
                        )
                        (Value.tryString queryVal |> Result.fromMaybe (Value.exception "query is not a string"))
                        (decodeQueryArgs argsVal)
                )
            )


decodeQueryArgs : Value io -> Result Exception (Dict String Lantern.Query.Value)
decodeQueryArgs argsVal =
    Value.tryDictOf
        (Value.tryOneOf [ Value.tryKeyword, Value.tryString ] >> Maybe.map ((++) "$"))
        (Value.tryOneOf
            [ Value.tryInt >> Maybe.map Lantern.Query.Integer
            , Value.tryFloat >> Maybe.map Lantern.Query.Real
            , Value.tryString >> Maybe.map Lantern.Query.Text
            , Value.tryNil >> Maybe.map (always Lantern.Query.Null)
            , Value.tryBool
                >> Maybe.map
                    (\bool ->
                        if bool then
                            Lantern.Query.Integer 1

                        else
                            Lantern.Query.Integer 0
                    )
            ]
        )
        argsVal
        |> Result.fromMaybe (Value.exception "args is not a map of keywords or strings to int, float, or string")


writerQueryResultToValue : Lantern.Query.WriterResult -> Value io
writerQueryResultToValue { changedRows, lastInsertRowId } =
    Enclojure.ValueMap.fromList
        [ ( Value.keyword "changed-rows"
          , Located.unknown <| Value.int changedRows
          )
        , ( Value.keyword "last-insert-row-id"
          , Located.unknown <| Value.int lastInsertRowId
          )
        ]
        |> Value.map


writerQuery : Callable MyIO
writerQuery =
    Callable.new
        |> Callable.setArity1
            (Callable.fixedArity
                (Value.symbol "query")
                (\val ->
                    val
                        |> Value.tryString
                        |> Result.fromMaybe (Value.exception "not a string")
                        |> Result.map
                            (\query ->
                                Lantern.writerQuery
                                    (Lantern.Query.withNoArguments query)
                                    (Result.mapError (Lantern.Errors.toString >> Value.exception)
                                        >> Result.map writerQueryResultToValue
                                    )
                                    |> LanternTask
                                    |> Runtime.sideEffect
                            )
                )
            )
        |> Callable.setArity2
            (Callable.fixedArity ( Value.symbol "query", Value.symbol "args" )
                (\( queryVal, argsVal ) ->
                    Result.map2
                        (\query args ->
                            Lantern.writerQuery
                                (Lantern.Query.withArguments query (Dict.toList args))
                                (Result.mapError (Lantern.Errors.toString >> Value.exception)
                                    >> Result.map writerQueryResultToValue
                                )
                                |> LanternTask
                                |> Runtime.sideEffect
                        )
                        (Value.tryString queryVal |> Result.fromMaybe (Value.exception "query is not a string"))
                        (decodeQueryArgs argsVal)
                )
            )


printlnFn : Callable MyIO
printlnFn =
    Callable.new
        |> Callable.setArity0
            (Callable.variadicArity
                { argNames = (), restArgName = Value.symbol "vals" }
                (\{ rest } ->
                    rest
                        |> List.map Value.toString
                        |> String.join " "
                        |> Println
                        |> Runtime.sideEffect
                        |> Ok
                )
            )


toTextPart : Value MyIO -> Result Exception TextFormat
toTextPart val =
    val
        |> Value.tryOneOf
            [ Value.tryString >> Maybe.map Plain
            , Value.tryVectorOf (identity >> Just)
                >> Maybe.andThen
                    (Value.tryPatternOf2
                        (\kw v rst ->
                            if not (List.isEmpty rst) then
                                Nothing

                            else
                                case kw of
                                    "$" ->
                                        v |> Maybe.map TextRef

                                    "md" ->
                                        v |> Maybe.andThen Value.tryString |> Maybe.map Markdown

                                    _ ->
                                        Nothing
                        )
                        Value.tryKeyword
                        (identity >> Just >> Just)
                    )
            ]
        |> Result.fromMaybe
            (Value.exception ("text parts must be plain strings or variable vectors (e.g., [:$ :var]), got: " ++ Value.inspect val))


toUi : Value MyIO -> Result Exception Cell
toUi val =
    if val == Value.nil then
        Ok Empty

    else
        val
            |> Value.tryVectorOf Just
            |> Result.fromMaybe (Value.exception "cell must be a vector")
            |> Result.andThen
                (\cell ->
                    if List.length cell > 0 then
                        Ok cell

                    else
                        Err (Value.exception "empty vector is not a valid cell")
                )
            |> Result.andThen
                (\v ->
                    let
                        args =
                            List.drop 1 v
                    in
                    v
                        |> List.head
                        |> Maybe.andThen Value.tryKeyword
                        |> Result.fromMaybe (Value.exception "type error: cell type must be a keyword")
                        |> Result.andThen
                            (\cellType ->
                                case cellType of
                                    "v-stack" ->
                                        args
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

                                    "h-stack" ->
                                        args
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

                                    "image" ->
                                        case args of
                                            [ imageUrlArg ] ->
                                                imageUrlArg
                                                    |> Value.tryString
                                                    |> Maybe.map (Image { width = Nothing })
                                                    |> Result.fromMaybe (Value.exception "image URL must be a string")

                                            [ optsArg, imageUrlArg ] ->
                                                Result.map2
                                                    Image
                                                    (optsArg
                                                        |> Value.tryDictOf Value.tryKeyword (identity >> Just)
                                                        |> Result.fromMaybe (Value.exception "image opts must be a map")
                                                        |> Result.andThen
                                                            (\optsMap ->
                                                                let
                                                                    width =
                                                                        Dict.get "width" optsMap
                                                                            |> Maybe.map (Value.tryInt >> Result.fromMaybe (Value.exception "image width must be an integer") >> Result.map Just)
                                                                            |> Maybe.withDefault (Ok Nothing)
                                                                in
                                                                Result.map (\w -> { width = w }) width
                                                            )
                                                    )
                                                    (imageUrlArg
                                                        |> Value.tryString
                                                        |> Result.fromMaybe (Value.exception "image URL must be a string")
                                                    )

                                            _ ->
                                                Err (Value.exception "Invalid number of parameters to :image")

                                    "text" ->
                                        args
                                            |> List.foldr (\e a -> toTextPart e |> Result.map2 (\x y -> y :: x) a) (Ok [])
                                            |> Result.map Text

                                    "text-input" ->
                                        args
                                            |> List.head
                                            |> Result.fromMaybe (Value.exception "missing required key argument to :text-input")
                                            |> Result.andThen
                                                (\key ->
                                                    let
                                                        options =
                                                            args
                                                                |> List.drop 1
                                                                |> List.head
                                                                |> Maybe.andThen Value.tryMap
                                                                |> Maybe.withDefault Enclojure.ValueMap.empty

                                                        suggestions =
                                                            options
                                                                |> Enclojure.ValueMap.get (Value.keyword "suggestions")
                                                                |> Maybe.map Located.getValue
                                                                |> Maybe.andThen (Value.trySequenceOf Value.tryString)
                                                                |> Maybe.withDefault []
                                                    in
                                                    Ok (Input key (TextInput { suggestions = suggestions }))
                                                )

                                    "checkbox" ->
                                        args
                                            |> List.head
                                            |> Result.fromMaybe (Value.exception "missing required key argument to :checkbox")
                                            |> Result.andThen
                                                (\key ->
                                                    case args of
                                                        [ _ ] ->
                                                            Ok (Input key (Checkbox { label = key |> Value.tryOneOf [ Value.tryString, Value.tryKeyword ] |> Maybe.withDefault "" }))

                                                        [ _, labelVal ] ->
                                                            labelVal
                                                                |> Value.tryString
                                                                |> Result.fromMaybe
                                                                    (Value.exception "checkbox label must be a string")
                                                                |> Result.map
                                                                    (\label ->
                                                                        Input key (Checkbox { label = label })
                                                                    )

                                                        _ ->
                                                            Err <| Value.exception ("invalid number of parameters (" ++ String.fromInt (List.length args) ++ ") passed to :checkbox")
                                                )

                                    "button" ->
                                        args
                                            |> List.head
                                            |> Result.fromMaybe (Value.exception "missing required key argument to :button")
                                            |> Result.andThen
                                                (\key ->
                                                    let
                                                        options =
                                                            args
                                                                |> List.drop 1
                                                                |> List.head
                                                                |> Maybe.andThen Value.tryMap
                                                                |> Maybe.withDefault Enclojure.ValueMap.empty

                                                        title =
                                                            Enclojure.ValueMap.get (Value.keyword "title") options
                                                                |> Maybe.map Located.getValue
                                                                |> Maybe.andThen Value.tryString
                                                                |> Maybe.withDefault
                                                                    (key
                                                                        |> Value.tryOneOf [ Value.tryString, Value.tryKeyword ]
                                                                        |> Maybe.withDefault "OK"
                                                                    )
                                                    in
                                                    Ok (Input key (Button { title = title }))
                                                )

                                    "download" ->
                                        args
                                            |> Value.tryPatternOf2
                                                (\key content restArgs ->
                                                    let
                                                        options =
                                                            restArgs
                                                                |> List.head
                                                                |> Maybe.andThen Value.tryMap
                                                                |> Maybe.withDefault Enclojure.ValueMap.empty

                                                        contentType =
                                                            Enclojure.ValueMap.get (Value.keyword "content-type") options
                                                                |> Maybe.map Located.getValue
                                                                |> Maybe.andThen Value.tryString
                                                                |> Maybe.withDefault "text/plain"

                                                        name =
                                                            Enclojure.ValueMap.get (Value.keyword "name") options
                                                                |> Maybe.map Located.getValue
                                                                |> Maybe.andThen Value.tryString
                                                                |> Maybe.withDefault "download.txt"
                                                    in
                                                    Just
                                                        (Input key
                                                            (Download
                                                                { name = name
                                                                , content = content
                                                                , contentType = contentType
                                                                }
                                                            )
                                                        )
                                                )
                                                (identity >> Just)
                                                Value.tryString
                                            |> Result.fromMaybe (Value.exception "type error: invalid arguments to download cell")

                                    _ ->
                                        Err (Value.exception ("type error: " ++ cellType ++ " is not a supported cell type"))
                            )
                )


ui : Callable MyIO
ui =
    let
        arity0 { rest } =
            let
                optsArg =
                    rest |> List.head |> Maybe.andThen Value.tryMap

                restArgs =
                    if optsArg == Nothing then
                        rest

                    else
                        List.drop 1 rest

                opts =
                    optsArg |> Maybe.withDefault Enclojure.ValueMap.empty
            in
            case restArgs of
                [ uiVal ] ->
                    arity3 uiVal Value.nil (Value.map Enclojure.ValueMap.empty) opts

                [ uiVal, defaultsMap ] ->
                    arity3 uiVal Value.nil defaultsMap opts

                [ uiVal, watchFn, defaultsMap ] ->
                    arity3 uiVal watchFn defaultsMap opts

                _ ->
                    Err
                        (Value.exception
                            ("ui called with invalid number of arguments ("
                                ++ String.fromInt (List.length rest)
                                ++ ")"
                            )
                        )

        arity3 uiVal watchFn defaultsMap opts =
            defaultsMap
                |> Value.tryMap
                |> Result.fromMaybe (Value.exception ("type error: expected a map of defaults, got " ++ Value.inspect defaultsMap))
                |> Result.andThen
                    (\m ->
                        toUi uiVal
                            |> Result.map
                                (\cell ->
                                    Runtime.sideEffect <|
                                        ShowUI
                                            { id =
                                                Enclojure.ValueMap.get (Value.keyword "id") opts
                                                    |> Maybe.map Located.getValue
                                                    |> Maybe.withDefault Value.nil
                                            , cell = cell
                                            , watchFn = watchFn
                                            , state = m
                                            }
                                )
                    )
    in
    Callable.new
        |> Callable.setArity0 (Callable.variadicArity { argNames = (), restArgName = Value.symbol "args" } arity0)


type MyIO
    = IOTask (Task.Task Exception (Value MyIO))
    | LanternTask (Task.Task Never (Lantern.Message (Result Exception (Value MyIO))))
    | ShowUI (UI MyIO)
    | Savepoint (Value MyIO)
    | Println String


type Message
    = FuzzySelectMessage InputKey LanternUi.FuzzySelect.Message
    | Run
    | Stop
    | UpdateInputRequest InputKey InputCell String
    | UpdateScripts (Result Lantern.Error (List ( ScriptId, Script )))
    | UpdateScript (Result Lantern.Error (Maybe ( ScriptId, Script )))
    | HandleIO (Step MyIO)
    | InspectValue (Value MyIO)
    | CreateScript
    | ScriptCreated (Result Lantern.Error Lantern.Query.WriterResult)
    | SaveScript
    | ScriptSaved (Result Lantern.Error Lantern.Query.WriterResult)
    | NewScript
    | DeleteScript ScriptId
    | EditScript ScriptId Script
    | ToggleMode
    | UpdateName String
    | UpdateCode String
    | UpdateRepl String
    | UpdateBrowserQuery String
    | Eval String
    | DownloadFile String String String
    | NoOp


init : Maybe Flags -> ( Model, Cmd (Lantern.App.Message Message) )
init =
    Maybe.withDefault BrowserMode
        >> (\flags ->
                case flags of
                    BrowserMode ->
                        ( Browser { scripts = [], query = "" }, Cmd.none )

                    EditScriptMode id ->
                        ( Editor
                            (LanternUi.Persistent.Loading id)
                        , Cmd.none
                        )

                    NewScriptMode ->
                        ( Editor (LanternUi.Persistent.New emptyEditor)
                        , Cmd.none
                        )

                    RunnerMode id ->
                        ( Runner (LanternUi.Persistent.Loading id), Cmd.none )
           )


emptyScript : { code : String, name : String, input : String }
emptyScript =
    { code = "", name = "", input = "" }


type alias UI io =
    { id : Value io
    , state : ValueMap io
    , cell : Cell
    , watchFn : Value io
    }


type alias UiModel =
    { fuzzySelects : ValueKeyMap MyIO FuzzySelect
    , enclojureUi : UI MyIO
    }


type Interpreter
    = Stopped
    | ShowingUI UiModel (Env MyIO) (Value MyIO -> Step MyIO)
    | Running
    | Done (Value MyIO) (Env MyIO)
    | Panic Exception (Env MyIO)


responseToValue : Lantern.Http.Response -> Value MyIO
responseToValue response =
    Value.map <|
        Enclojure.ValueMap.fromList
            [ ( Value.keyword "status", Located.unknown <| Value.int response.status )
            , ( Value.keyword "headers"
              , Located.unknown <|
                    Value.map
                        (response.headers
                            |> List.map (\( k, v ) -> ( Value.string k, Located.unknown (Value.string v) ))
                            |> Enclojure.ValueMap.fromList
                        )
              )
            , ( Value.keyword "body"
              , response.body |> Maybe.map Value.string |> Maybe.withDefault Value.nil |> Located.unknown
              )
            ]


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
        Editor p ->
            let
                ( newM, cmd ) =
                    LanternUi.Persistent.state p
                        |> Maybe.map (updateFn >> Tuple.mapFirst (\s -> LanternUi.Persistent.mapState (always s) p))
                        |> Maybe.withDefault ( p, Cmd.none )
            in
            ( Editor newM, cmd )

        Runner p ->
            let
                ( newM, cmd ) =
                    LanternUi.Persistent.state p
                        |> Maybe.map (updateFn >> Tuple.mapFirst (\s -> LanternUi.Persistent.mapState (always s) p))
                        |> Maybe.withDefault ( p, Cmd.none )
            in
            ( Runner newM, cmd )

        _ ->
            ( model, Cmd.none )


updatePersistentEditor :
    Model
    ->
        (Persistent ScriptId EditorModel
         -> Bool -- isRunning
         -> ( Persistent ScriptId EditorModel, Cmd (Lantern.App.Message Message) )
        )
    -> ( Model, Cmd (Lantern.App.Message Message) )
updatePersistentEditor model updateFn =
    case model of
        Editor m ->
            let
                ( newM, cmd ) =
                    updateFn m False
            in
            ( Editor newM, cmd )

        Runner m ->
            let
                ( newM, cmd ) =
                    updateFn m True
            in
            ( Runner newM, cmd )

        _ ->
            ( model, Cmd.none )


runWatchFn :
    Env MyIO
    -> Value MyIO
    -> ValueMap MyIO
    -> Result Exception (ValueMap MyIO)
runWatchFn env watchFn stateMap =
    case Value.tryNil watchFn of
        Just _ ->
            Ok stateMap

        _ ->
            let
                ( evalResult, _ ) =
                    Enclojure.continueEval
                        { maxOps = Just stackLimit }
                        (Runtime.apply (Located.unknown watchFn)
                            (Located.unknown (Value.list [ Value.map stateMap ]))
                            env
                            Runtime.terminate
                        )
            in
            case evalResult of
                Enclojure.Done val ->
                    val
                        |> Value.tryMap
                        |> Result.fromMaybe (Value.exception "type error: watch returned a non-map")

                Enclojure.Error err ->
                    Err err

                Enclojure.RunIO _ _ ->
                    Err (Value.exception "runtime error: watch fn tried to run a side effect")

                Enclojure.Continue _ ->
                    Err (Value.exception "runtime error: watch fn ran out of ops")


defaultEnv : Env MyIO
defaultEnv =
    Enclojure.init
        |> Runtime.bindGlobal "http/request"
            (Value.fn (Just "http/request") http)
        |> Runtime.bindGlobal "lantern/echo"
            (Value.fn (Just "lantern/echo") echo)
        |> Runtime.bindGlobal "lantern/reader-query"
            (Value.fn (Just "lantern/reader-query") readerQuery)
        |> Runtime.bindGlobal "lantern/writer-query"
            (Value.fn (Just "lantern/writer-query") writerQuery)
        |> Runtime.bindGlobal "println"
            (Value.fn (Just "println") printlnFn)
        |> Runtime.bindGlobal "sleep"
            (Value.fn (Just "sleep") sleep)
        |> Runtime.bindGlobal "ui"
            (Value.fn (Just "ui") ui)
        |> Runtime.bindGlobal "<o>"
            (Value.fn (Just "<o>") savepoint)


handleEvalResult :
    EvalResult MyIO
    -> { env : Env MyIO, console : Console }
    -> ( Interpreter, Console, Cmd (Lantern.App.Message Message) )
handleEvalResult evalResult { env, console } =
    case evalResult of
        Enclojure.Done val ->
            ( Done val env, console |> printResult (Done val env), Cmd.none )

        Enclojure.Error err ->
            ( Panic err env, console |> printResult (Panic err env), Cmd.none )

        Enclojure.Continue step ->
            ( Running, console, Task.perform identity (Task.succeed (Lantern.App.Message <| HandleIO step)) )

        Enclojure.RunIO se toStep ->
            case se of
                IOTask task ->
                    ( Running
                    , console
                    , task
                        |> Task.attempt
                            (\r ->
                                toStep r
                                    |> HandleIO
                                    |> Lantern.App.Message
                            )
                    )

                LanternTask task ->
                    ( Running
                    , console
                    , task
                        |> Task.map (Lantern.map (toStep >> HandleIO))
                        |> Lantern.App.call
                    )

                Println str ->
                    ( Running
                    , str |> String.lines |> List.foldr printLn console
                    , toStep (Ok Value.nil)
                        |> HandleIO
                        |> Lantern.App.Message
                        |> Task.succeed
                        |> Task.perform identity
                    )

                ShowUI uiState ->
                    ( ShowingUI
                        { enclojureUi = uiState
                        , fuzzySelects = Enclojure.ValueKeyMap.empty
                        }
                        env
                        (Ok >> toStep)
                    , console
                    , Cmd.none
                    )

                Savepoint v ->
                    ( Running
                    , console
                    , toStep (Ok v)
                        |> HandleIO
                        |> Lantern.App.Message
                        |> Task.succeed
                        |> Task.perform identity
                    )


runScript : EditorModel -> ( EditorModel, Cmd (Lantern.App.Message Message) )
runScript model =
    let
        ( evalResult, retEnv ) =
            Enclojure.eval { maxOps = Just stackLimit } defaultEnv model.script.code

        ( interpreter, console, retCmd ) =
            handleEvalResult evalResult
                { env = retEnv
                , console = model.console |> printLn ("Starting " ++ scriptName model.script)
                }
    in
    ( { model
        | interpreter = interpreter
        , console = console |> printResult interpreter
      }
    , retCmd
    )


currentFrameName : Env io -> String
currentFrameName { stack } =
    stack
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "<unknown>"


update : Message -> Model -> ( Model, Cmd (Lantern.App.Message Message) )
update msg appModel =
    case msg of
        DownloadFile filename contentType content ->
            ( appModel, File.Download.string filename contentType content )

        DeleteScript id ->
            updateBrowser appModel
                (\model ->
                    Lantern.Query.withArguments
                        "DELETE FROM scripts WHERE id=$id"
                        [ ( "$id", id |> Lantern.Query.Integer )
                        ]
                        |> (\query -> ( model, Lantern.writerQuery query ScriptSaved |> Lantern.App.call ))
                )

        FuzzySelectMessage id selectMsg ->
            updateEditor appModel
                (\model ->
                    case model.interpreter of
                        ShowingUI ({ fuzzySelects } as uiState) env toStep ->
                            let
                                updatedFuzzySelect =
                                    Enclojure.ValueKeyMap.get id fuzzySelects
                                        |> Maybe.withDefault LanternUi.FuzzySelect.hidden
                                        |> LanternUi.FuzzySelect.update selectMsg
                            in
                            ( { model
                                | interpreter =
                                    ShowingUI
                                        { uiState
                                            | fuzzySelects = Enclojure.ValueKeyMap.insert id updatedFuzzySelect fuzzySelects
                                        }
                                        env
                                        toStep
                              }
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
                                Done _ env ->
                                    Just env

                                Panic _ env ->
                                    Just env

                                _ ->
                                    Nothing
                    in
                    mEnv
                        |> Maybe.map
                            (\env ->
                                let
                                    ( evalResult, retEnv ) =
                                        Enclojure.eval { maxOps = Just stackLimit } env code

                                    ( interpreter, console, retCmd ) =
                                        handleEvalResult evalResult
                                            { env = retEnv
                                            , console = model.console |> printLn "Evaluating from REPL"
                                            }
                                in
                                ( { model
                                    | interpreter = interpreter
                                    , console = console |> printResult interpreter
                                    , repl = ""
                                  }
                                , retCmd
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
                        | interpreter = Panic (Value.exception "Terminated") Runtime.emptyEnv
                      }
                    , Cmd.none
                    )
                )

        HandleIO step ->
            updateEditor appModel
                (\model ->
                    let
                        ( evalResult, retEnv ) =
                            Enclojure.continueEval { maxOps = Just stackLimit } step

                        ( interpreter, console, retCmd ) =
                            handleEvalResult evalResult
                                { env = retEnv, console = recordSavepoint step model.console }
                    in
                    ( { model | interpreter = interpreter, console = console }, retCmd )
                )

        UpdateInputRequest name inputType v ->
            updateEditor
                appModel
                (\model ->
                    case model.interpreter of
                        ShowingUI ({ enclojureUi } as uiState) env toStep ->
                            case inputType of
                                Button _ ->
                                    let
                                        exitCode =
                                            name

                                        state =
                                            Value.map enclojureUi.state

                                        console =
                                            model.console
                                                |> traceUi uiState
                                                |> printResult model.interpreter
                                    in
                                    ( { model | interpreter = Running, console = console }
                                    , Value.list [ exitCode, state ]
                                        |> toStep
                                        |> HandleIO
                                        |> Lantern.App.Message
                                        |> Task.succeed
                                        |> Task.perform identity
                                    )

                                Checkbox _ ->
                                    let
                                        currentValue =
                                            Enclojure.ValueKeyMap.get name enclojureUi.state
                                                |> Maybe.map Located.getValue
                                                |> Maybe.withDefault (Value.boolean False)

                                        newValue =
                                            if Value.isTruthy currentValue then
                                                Value.boolean False

                                            else
                                                Value.boolean True

                                        updatedState =
                                            Enclojure.ValueKeyMap.insert name (Located.unknown newValue) enclojureUi.state
                                                |> runWatchFn env uiState.enclojureUi.watchFn

                                        ( interpreter, console ) =
                                            case updatedState of
                                                Ok st ->
                                                    let
                                                        updatedUi =
                                                            { enclojureUi | state = st }
                                                    in
                                                    ( ShowingUI { uiState | enclojureUi = updatedUi } env toStep
                                                    , model.console
                                                    )

                                                Err ex ->
                                                    let
                                                        i =
                                                            Panic ex env
                                                    in
                                                    ( i, printResult i model.console )

                                        updatedModel =
                                            { model | interpreter = interpreter, console = console }
                                    in
                                    ( updatedModel
                                    , Cmd.none
                                    )

                                _ ->
                                    let
                                        updatedState =
                                            Enclojure.ValueMap.insert name
                                                (Located.unknown (Value.string v))
                                                uiState.enclojureUi.state
                                                |> runWatchFn env uiState.enclojureUi.watchFn

                                        ( interpreter, console ) =
                                            case updatedState of
                                                Ok st ->
                                                    let
                                                        updatedUi =
                                                            { enclojureUi | state = st }
                                                    in
                                                    ( ShowingUI { uiState | enclojureUi = updatedUi } env toStep
                                                    , model.console
                                                    )

                                                Err ex ->
                                                    let
                                                        i =
                                                            Panic ex env
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

        UpdateScript result ->
            updatePersistentEditor appModel
                (\model isRunning ->
                    result
                        |> Result.toMaybe
                        |> Maybe.andThen identity
                        |> Maybe.map
                            (\( _, script ) ->
                                case model of
                                    LanternUi.Persistent.Loaded id editor ->
                                        ( LanternUi.Persistent.Loaded id { editor | serverScript = Just script }, Cmd.none )

                                    LanternUi.Persistent.Loading id ->
                                        let
                                            editorModel =
                                                { emptyEditor | script = script, serverScript = Just script }
                                        in
                                        if isRunning then
                                            runScript editorModel |> Tuple.mapFirst (LanternUi.Persistent.Loaded id)

                                        else
                                            ( LanternUi.Persistent.Loaded id editorModel, Cmd.none )

                                    LanternUi.Persistent.New _ ->
                                        ( model, Cmd.none )
                            )
                        |> Maybe.withDefault ( model, Cmd.none )
                )

        SaveScript ->
            updatePersistentEditor appModel
                (\model _ ->
                    let
                        cmd =
                            Maybe.map2
                                (\id s ->
                                    Lantern.Query.withArguments
                                        "UPDATE scripts SET name=$name, code=$code, input=$input, updated_at=datetime('now') WHERE id=$id"
                                        [ ( "$name", s.name |> Lantern.Query.Text )
                                        , ( "$code", s.code |> Lantern.Query.Text )
                                        , ( "$input", s.input |> Lantern.Query.Text )
                                        , ( "$id", id |> Lantern.Query.Integer )
                                        ]
                                )
                                (LanternUi.Persistent.id model)
                                (LanternUi.Persistent.state model |> Maybe.map .script)
                                |> Maybe.map (\query -> Lantern.writerQuery query ScriptSaved |> Lantern.App.call)
                                |> Maybe.withDefault Cmd.none
                    in
                    ( model, cmd )
                )

        NewScript ->
            ( Editor
                (LanternUi.Persistent.New emptyEditor)
            , Lantern.App.reflag
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
                                [ ( "$name", model.script.name |> Lantern.Query.Text )
                                , ( "$code", model.script.code |> Lantern.Query.Text )
                                , ( "$input", model.script.input |> Lantern.Query.Text )
                                ]
                    in
                    ( model, Lantern.writerQuery query ScriptCreated |> Lantern.App.call )
                )

        ScriptCreated result ->
            updatePersistentEditor appModel
                (\pModel _ ->
                    result
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\writerResult ->
                                case pModel of
                                    LanternUi.Persistent.New model ->
                                        Just ( LanternUi.Persistent.Loaded writerResult.lastInsertRowId model, Lantern.App.reflag )

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.withDefault ( pModel, Cmd.none )
                )

        EditScript id script ->
            ( Editor (LanternUi.Persistent.Loaded id { emptyEditor | script = script })
            , Lantern.App.reflag
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

        ToggleMode ->
            ( case appModel of
                Runner m ->
                    Editor m

                Editor m ->
                    Runner m

                Browser _ ->
                    appModel
            , Cmd.none
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
        Empty ->
            Element.none

        VStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { id = uiModel.enclojureUi.id, cell = c, watchFn = watchFn, state = state } })
                |> Element.column [ Element.width Element.fill, Element.alignTop, Element.spacing 10 ]

        HStack cells ->
            cells
                |> List.map (\c -> renderUI context { uiModel | enclojureUi = { id = uiModel.enclojureUi.id, cell = c, watchFn = watchFn, state = state } })
                |> Element.row
                    [ Element.width Element.fill
                    , Element.spacing 10
                    , Element.alignTop
                    ]

        Input key inputType ->
            let
                val =
                    Enclojure.ValueMap.get key state
                        |> Maybe.map (Located.getValue >> Value.toString)
                        |> Maybe.withDefault ""
            in
            case inputType of
                TextInput opts ->
                    if List.isEmpty opts.suggestions then
                        LanternUi.Input.multiline
                            context.theme
                            [ Element.width Element.fill
                            , Element.alignTop
                            ]
                            { onChange = UpdateInputRequest key inputType >> Lantern.App.Message
                            , placeholder = Nothing
                            , label = Element.Input.labelHidden (Value.inspect key)
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
                            , state =
                                Enclojure.ValueKeyMap.get key fuzzySelects
                                    |> Maybe.withDefault LanternUi.FuzzySelect.hidden
                            , id = Nothing
                            }

                MaskedTextInput ->
                    LanternUi.Input.password
                        context.theme
                        [ Element.width Element.fill, Element.alignTop ]
                        { onChange = UpdateInputRequest key inputType >> Lantern.App.Message
                        , placeholder = Nothing
                        , label = Element.Input.labelHidden ""
                        , text = val
                        , show = False
                        }

                Checkbox { label } ->
                    Element.Input.checkbox
                        []
                        { onChange = always (UpdateInputRequest key inputType "" |> Lantern.App.Message)
                        , icon = Element.Input.defaultCheckbox
                        , checked =
                            uiModel.enclojureUi.state
                                |> Enclojure.ValueKeyMap.get key
                                |> Maybe.map (Located.getValue >> Value.isTruthy)
                                |> Maybe.withDefault False
                        , label = Element.Input.labelRight [] (Element.text label)
                        }

                Button { title } ->
                    LanternUi.Input.button
                        context.theme
                        [ Element.alignTop ]
                        { onPress = Just (UpdateInputRequest key inputType "" |> Lantern.App.Message)
                        , label = Element.text title
                        }

                Download { name, contentType, content } ->
                    LanternUi.Input.button
                        context.theme
                        [ Element.alignTop ]
                        { onPress = Just (DownloadFile name contentType content |> Lantern.App.Message)
                        , label = Element.text "Download"
                        }

        Image options url ->
            Element.el
                [ Element.width Element.fill
                , Element.alignTop
                ]
                (Element.image
                    [ options.width
                        |> Maybe.map (Element.px >> Element.width)
                        |> Maybe.withDefault LanternUi.noneAttribute
                    ]
                    { description = ""
                    , src = url
                    }
                )

        Text textCells ->
            textCells
                |> List.map
                    (\c ->
                        case c of
                            Plain t ->
                                Element.text t

                            Markdown t ->
                                Element.html (Markdown.toHtml [] t)

                            TextRef key ->
                                let
                                    val =
                                        Enclojure.ValueMap.get key state
                                            |> Maybe.map (Located.getValue >> Value.toString)
                                            |> Maybe.withDefault ""
                                in
                                Element.text val
                    )
                |> Element.paragraph [ Element.alignTop ]


activeUi : Interpreter -> Maybe UiModel
activeUi interpreter =
    case interpreter of
        ShowingUI model _ _ ->
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


dedupeUis : Maybe (Value MyIO) -> List ConsoleEntry -> List ConsoleEntry
dedupeUis lastUiId entries =
    case entries of
        ((UiTrace uiModel) as entry) :: rest ->
            case lastUiId of
                Just id ->
                    if uiModel.enclojureUi.id /= Value.nil then
                        if Value.isEqual uiModel.enclojureUi.id id then
                            dedupeUis lastUiId rest

                        else
                            entry :: dedupeUis (Just uiModel.enclojureUi.id) rest

                    else
                        entry :: dedupeUis Nothing rest

                Nothing ->
                    entry :: dedupeUis (Just uiModel.enclojureUi.id) rest

        entry :: rest ->
            entry :: dedupeUis lastUiId rest

        [] ->
            []


viewConsole : Context -> Interpreter -> Console -> ConsoleOptions -> Element (Lantern.App.Message Message)
viewConsole context interpreter console options =
    activeUi interpreter
        |> Maybe.map (\uiState -> UiTrace uiState :: console)
        |> Maybe.withDefault console
        |> dedupeUis Nothing
        |> List.filter (\entry -> options.devMode || not (isDevModeOnlyEntry entry))
        |> List.take 20
        |> List.indexedMap
            (\i entry ->
                let
                    valueRow val =
                        Element.paragraph
                            [ Element.width Element.fill ]
                            [ Element.text (Value.inspectType val)
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

                        Savepoint_ step ->
                            let
                                env =
                                    Enclojure.getStepEnv step

                                newEnv =
                                    case interpreter of
                                        Done _ cEnv ->
                                            { cEnv | lexicalScope = env.lexicalScope }

                                        Panic _ cEnv ->
                                            { cEnv | lexicalScope = env.lexicalScope }

                                        _ ->
                                            env

                                rewindButton =
                                    LanternUi.Input.button context.theme
                                        []
                                        { onPress =
                                            Just
                                                (step
                                                    |> Enclojure.setStepEnv newEnv
                                                    |> HandleIO
                                                    |> Lantern.App.Message
                                                )
                                        , label = Element.text "Rewind"
                                        }
                            in
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.paragraph [] [ Element.text ("Savepoint (" ++ currentFrameName env ++ ")"), rewindButton ]
                                , Enclojure.getStepValue step |> Maybe.map valueRow |> Maybe.withDefault Element.none
                                ]

                        Success val ->
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.paragraph [ Element.width Element.fill ] [ Element.text "Done" ]
                                , valueRow val
                                ]

                        Failure ( e, _ ) ->
                            Element.column
                                [ Element.width Element.fill ]
                                [ Element.text <| Value.inspect (Value.throwable e)
                                , List.map Element.text (Runtime.prettyTrace e) |> Element.column []
                                ]

                        UiTrace uiState ->
                            Element.column
                                [ Element.width Element.fill
                                ]
                                [ renderUI context uiState ]
                    )
            )
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            , Element.above
                (Element.el [ Element.alignRight ]
                    (LanternUi.Input.button context.theme
                        []
                        { label =
                            Element.text
                                (if options.devMode then
                                    "Maximize"

                                 else
                                    "Debug"
                                )
                        , onPress = Just <| Lantern.App.Message ToggleMode
                        }
                    )
                )
            ]


viewEditor : Context -> Maybe ScriptId -> EditorModel -> Element (Lantern.App.Message Message)
viewEditor context mScriptId model =
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
            if mScriptId == Nothing then
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
                    (\( _, s ) ->
                        String.isEmpty model.query
                            || String.contains (String.toLower model.query) (String.toLower s.name)
                    )

        scriptsList =
            filteredScripts
                |> List.map
                    (\( scriptId, script ) ->
                        Element.row
                            [ Element.width Element.fill
                            , Element.spacing 20
                            ]
                            [ Element.Input.button
                                [ Element.mouseOver [ Element.Background.color context.theme.bgHighlight ] ]
                                { onPress = Just (Lantern.App.Message <| EditScript scriptId script)
                                , label = Element.text (scriptName script)
                                }
                            , LanternUi.Input.button context.theme
                                []
                                { onPress = Just (Lantern.App.Message <| DeleteScript scriptId)
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


viewRunner : Context -> Maybe ScriptId -> EditorModel -> Element (Lantern.App.Message Message)
viewRunner context _ model =
    viewConsole context model.interpreter model.console { devMode = False }


view : Context -> Model -> Element (Lantern.App.Message Message)
view context model =
    case model of
        Browser browser ->
            viewBrowser context browser

        Editor editor ->
            LanternUi.loader (viewEditor context) editor

        Runner runner ->
            LanternUi.loader (viewRunner context) runner


type alias Script =
    { name : String
    , code : String
    , input : String
    }


scriptDecoder : Json.Decode.Decoder ( ScriptId, Script )
scriptDecoder =
    Json.Decode.map4
        (\id name code input -> ( id, Script name code input ))
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "input" Json.Decode.string)


scriptsQuery : (Result Lantern.Error (List ( ScriptId, Script )) -> msg) -> LiveQuery msg
scriptsQuery =
    Lantern.LiveQuery.prepare
        ( Lantern.Query.Query "SELECT id, input, name, code FROM scripts ORDER BY name" Dict.empty, scriptDecoder )


scriptQuery : ScriptId -> (Result Lantern.Error (Maybe ( ScriptId, Script )) -> msg) -> LiveQuery msg
scriptQuery id toMsg =
    Lantern.LiveQuery.prepare
        ( Lantern.Query.withArguments "SELECT id, input, name, code FROM scripts WHERE id=$id ORDER BY name"
            [ ( "$id", Lantern.Query.Integer id ) ]
        , scriptDecoder
        )
        (Result.map List.head >> toMsg)


liveQueries : Model -> List (LiveQuery Message)
liveQueries model =
    case model of
        Browser _ ->
            [ scriptsQuery UpdateScripts ]

        Editor ps ->
            ps
                |> LanternUi.Persistent.id
                |> Maybe.map (\id -> [ scriptQuery id UpdateScript ])
                |> Maybe.withDefault []

        Runner ps ->
            ps
                |> LanternUi.Persistent.id
                |> Maybe.map (\id -> [ scriptQuery id UpdateScript ])
                |> Maybe.withDefault []


lanternApp : Lantern.App.App Context Flags Model Message
lanternApp =
    Lantern.App.app
        { name = "Scripts"
        , init = init
        , view = view
        , titleBarAddOns = \_ _ -> Element.none
        , update = update
        , liveQueries = Just liveQueries
        , subscriptions = always Sub.none
        , decodeFlags = Json.Decode.decodeValue flagsDecoder >> Result.toMaybe >> Maybe.andThen identity
        , encodeFlags = encodeFlags
        }
