module API (..) where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Quiz =
  { quizId : Int
  , quizName : String
  , quizDescription : String
  }


decodeQuiz : Json.Decode.Decoder Quiz
decodeQuiz =
  Json.Decode.succeed Quiz
    |: ("quizId" := Json.Decode.int)
    |: ("quizName" := Json.Decode.string)
    |: ("quizDescription" := Json.Decode.string)


encodeQuiz : Quiz -> Json.Encode.Value
encodeQuiz x =
  Json.Encode.object
    [ ( "quizId", Json.Encode.int x.quizId )
    , ( "quizName", Json.Encode.string x.quizName )
    , ( "quizDescription", Json.Encode.string x.quizDescription )
    ]


getQuizzes : Task.Task Http.Error (List (Quiz))
getQuizzes =
  let
    request =
      { verb =
          "GET"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/" ++ "quizzes"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeQuiz)
      (Http.send Http.defaultSettings request)


getQuizzesById : Int -> Task.Task Http.Error (Quiz)
getQuizzesById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "quizzes"
            ++ "/"
            ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeQuiz
      (Http.send Http.defaultSettings request)


postQuizzes : Quiz -> Task.Task Http.Error (Quiz)
postQuizzes body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/" ++ "quizzes"
      , body =
          Http.string (Json.Encode.encode 0 (encodeQuiz body))
      }
  in
    Http.fromJson
      decodeQuiz
      (Http.send Http.defaultSettings request)


emptyResponseHandler : a -> String -> Task.Task Http.Error a
emptyResponseHandler x str =
  if str == "[]" then
    Task.succeed x
  else
    Task.fail (Http.UnexpectedPayload str)


handleResponse : (String -> Task.Task Http.Error a) -> Http.Response -> Task.Task Http.Error a
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        handle str

      _ ->
        Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    Task.fail (Http.BadResponse response.status response.statusText)


promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout ->
      Http.Timeout

    Http.RawNetworkError ->
      Http.NetworkError


deleteQuizzesById : Int -> Task.Task Http.Error ()
deleteQuizzesById id =
  let
    request =
      { verb =
          "DELETE"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "quizzes"
            ++ "/"
            ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Task.mapError
      promoteError
      (Http.send Http.defaultSettings request)
      `Task.andThen` handleResponse (emptyResponseHandler ())


putQuizzesById : Int -> Quiz -> Task.Task Http.Error (Quiz)
putQuizzesById id body =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "quizzes"
            ++ "/"
            ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.string (Json.Encode.encode 0 (encodeQuiz body))
      }
  in
    Http.fromJson
      decodeQuiz
      (Http.send Http.defaultSettings request)


type alias Quizlet =
  { quizletId : Int
  , quizletQuizId : Int
  , quizletQuestion : String
  , quizletAnswer : String
  }


decodeQuizlet : Json.Decode.Decoder Quizlet
decodeQuizlet =
  Json.Decode.succeed Quizlet
    |: ("quizletId" := Json.Decode.int)
    |: ("quizletQuizId" := Json.Decode.int)
    |: ("quizletQuestion" := Json.Decode.string)
    |: ("quizletAnswer" := Json.Decode.string)


encodeQuizlet : Quizlet -> Json.Encode.Value
encodeQuizlet x =
  Json.Encode.object
    [ ( "quizletId", Json.Encode.int x.quizletId )
    , ( "quizletQuizId", Json.Encode.int x.quizletQuizId )
    , ( "quizletQuestion", Json.Encode.string x.quizletQuestion )
    , ( "quizletAnswer", Json.Encode.string x.quizletAnswer )
    ]


getQuizzesByQuizIdQuizlets : Int -> Task.Task Http.Error (List (Quizlet))
getQuizzesByQuizIdQuizlets quizId =
  let
    request =
      { verb =
          "GET"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "http://localhost:8081/"
            ++ "quizzes"
            ++ "/"
            ++ (quizId |> toString |> Http.uriEncode)
            ++ "/"
            ++ "quizlets"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeQuizlet)
      (Http.send Http.defaultSettings request)


getQuizletsById : Int -> Task.Task Http.Error (Quizlet)
getQuizletsById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "/"
            ++ "quizlets"
            ++ "/"
            ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeQuizlet
      (Http.send Http.defaultSettings request)


postQuizlets : Quizlet -> Task.Task Http.Error (Int)
postQuizlets body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [ ( "Content-Type", "application/json" ) ]
      , url =
          "/" ++ "quizlets"
      , body =
          Http.string (Json.Encode.encode 0 (encodeQuizlet body))
      }
  in
    Http.fromJson
      Json.Decode.int
      (Http.send Http.defaultSettings request)
