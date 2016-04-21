module Quiz.Actions (..) where

import Effects
import Task
import Http
import Hop
import API exposing (Quiz, getQuizzes)
import Quiz.Models exposing (QuizId)


type Action = NoOp
            | EditQuiz QuizId
            | ListQuizzes
            | GetQuizzesDone (Result Http.Error (List Quiz))
            | HopAction ()

getAllQuizzes : Effects.Effects Action
getAllQuizzes =
  getQuizzes
    |> Task.toResult
    |> Task.map GetQuizzesDone
    |> Effects.task
