module Generated.API where

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Quiz =
  { quizName : String
  , quizDescription : String
  }

decodeQuiz : Json.Decode.Decoder Quiz
decodeQuiz =
  Json.Decode.succeed Quiz
    |: ("quizName" := Json.Decode.string)
    |: ("quizDescription" := Json.Decode.string)

encodeQuiz : Quiz -> Json.Encode.Value
encodeQuiz x =
  Json.Encode.object
    [ ( "quizName", Json.Encode.string x.quizName )
    , ( "quizDescription", Json.Encode.string x.quizDescription )
    ]

getQuizzes : Task.Task Http.Error (List (Quiz))
getQuizzes =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizzes"
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
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizzes"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeQuiz
      (Http.send Http.defaultSettings request)

postQuizzes : Quiz -> Task.Task Http.Error (Int)
postQuizzes body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizzes"
      , body =
          Http.string (Json.Encode.encode 0 (encodeQuiz body))
      }
  in
    Http.fromJson
      Json.Decode.int
      (Http.send Http.defaultSettings request)

type alias Quizlet =
  { quizletQuizId : Int
  , quizletQuestion : String
  , quizletAnswer : String
  }

decodeQuizlet : Json.Decode.Decoder Quizlet
decodeQuizlet =
  Json.Decode.succeed Quizlet
    |: ("quizletQuizId" := Json.Decode.int)
    |: ("quizletQuestion" := Json.Decode.string)
    |: ("quizletAnswer" := Json.Decode.string)

encodeQuizlet : Quizlet -> Json.Encode.Value
encodeQuizlet x =
  Json.Encode.object
    [ ( "quizletQuizId", Json.Encode.int x.quizletQuizId )
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
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizzes"
          ++ "/" ++ (quizId |> toString |> Http.uriEncode)
          ++ "/" ++ "quizlets"
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
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizlets"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
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
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "quizlets"
      , body =
          Http.string (Json.Encode.encode 0 (encodeQuizlet body))
      }
  in
    Http.fromJson
      Json.Decode.int
      (Http.send Http.defaultSettings request)