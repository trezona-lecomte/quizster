module Quizlet.Actions (..) where

import Effects
import Http
import Task
import API exposing (Quizlet, getQuizzesByQuizIdQuizlets)
import Quizlet.Models exposing (..)
import Quiz.Models exposing (QuizId)


type Action
  = NoOp
  | TaskDone ()
  | HopAction ()
  | GetQuizlets Int
  | GetQuizletsDone (Result Http.Error (List Quizlet))
  | SubmitAnswer QuizletId String


getAllQuizlets : QuizId -> Effects.Effects Action
getAllQuizlets quizId =
  getQuizzesByQuizIdQuizlets quizId
    |> Task.toResult
    |> Task.map GetQuizletsDone
    |> Effects.task
