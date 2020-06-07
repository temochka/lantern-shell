module Lantern.Http exposing (Error(..), Request, RequestPayload, Response, errorToString, expectJson, expectString, requestPayload)

import Json.Decode


type alias Expect msg =
    Response -> msg


type Error
    = BadStatus Int
    | BadBody String


requireSuccess : Response -> Result Error Response
requireSuccess ({ status } as response) =
    if status >= 200 && status < 300 then
        Ok response

    else
        Err (BadStatus response.status)


extractBody : Response -> Result Error String
extractBody { body } =
    case body of
        Just text ->
            Ok text

        Nothing ->
            Err (BadBody "Empty body")


expectJson : (Result Error a -> msg) -> Json.Decode.Decoder a -> Expect msg
expectJson msg decoder =
    let
        decode body =
            case Json.Decode.decodeString decoder body of
                Ok result ->
                    Ok result

                Err error ->
                    Err (BadBody (Json.Decode.errorToString error))
    in
    requireSuccess
        >> Result.andThen extractBody
        >> Result.andThen decode
        >> msg


expectString : (Result Error String -> msg) -> Expect msg
expectString msg =
    requireSuccess
        >> Result.andThen extractBody
        >> msg


requestPayload : Request msg -> RequestPayload
requestPayload { method, headers, url, body } =
    { method = method, headers = headers, url = url, body = body }


errorToString : Error -> String
errorToString error =
    case error of
        BadBody msg ->
            msg

        BadStatus msg ->
            "Unexpected status" ++ String.fromInt msg


type alias RequestPayload =
    { method : String
    , headers : List ( String, String )
    , url : String
    , body : Maybe String
    }


type alias WithExpect a msg =
    { a | expect : Expect msg }


type alias Request msg =
    WithExpect RequestPayload msg


type alias Response =
    { status : Int
    , headers : List ( String, String )
    , body : Maybe String
    }
