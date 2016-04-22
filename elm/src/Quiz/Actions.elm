module Quiz.Actions (..) where

import Effects
import Task
import Http
import Hop
import API exposing (Quiz
                    , getQuizzes
                    , postQuizzes
                    , deleteQuizzesById
                    )
import Quiz.Models exposing (QuizId)


type Action = NoOp
            | EditQuiz QuizId
            | ListQuizzes
            | GetQuizzesDone (Result Http.Error (List Quiz))
            | CreateQuiz
            | CreateQuizDone (Result Http.Error Quiz)
            | DeleteQuizIntent Quiz
            | DeleteQuiz QuizId
            | DeleteQuizDone QuizId (Result Http.Error ())
            | TaskDone ()
            | HopAction ()

getAllQuizzes : Effects.Effects Action
getAllQuizzes =
  getQuizzes
    |> Task.toResult
    |> Task.map GetQuizzesDone
    |> Effects.task

createQuiz : Quiz -> Effects.Effects Action
createQuiz quiz =
  postQuizzes quiz
    |> Task.toResult
    |> Task.map CreateQuizDone
    |> Effects.task

deleteQuiz : QuizId -> Effects.Effects Action
deleteQuiz quizId =
  deleteQuizzesById quizId
    |> Task.toResult
    |> Task.map (DeleteQuizDone quizId)
    |> Effects.task
