module Quiz.Actions (..) where

import Effects
import Http
import Task
import Hop
import API exposing (Quiz, Quizlet, getQuizzes, postQuizzes, deleteQuizzesById, putQuizzesById, getQuizzesByQuizIdQuizlets)
import Quiz.Models exposing (QuizId)


type Action
  = NoOp
  | TaskDone ()
  | HopAction ()
  | GetQuizzes
  | GetQuizzesDone (Result Http.Error (List Quiz))
  | CreateQuiz
  | CreateQuizDone (Result Http.Error Quiz)
  | DeleteQuizIntent Quiz
  | DeleteQuiz QuizId
  | DeleteQuizDone QuizId (Result Http.Error ())
  | ChangeQuizName QuizId String
  | UpdateQuizDone (Result Http.Error Quiz)
  | ChangeQuizDescription QuizId String
  | AttemptQuiz QuizId
  | EditQuiz QuizId
  | GetQuizlets QuizId
  | GetQuizletsDone (Result Http.Error (List Quizlet))


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


updateQuiz : Quiz -> Effects.Effects Action
updateQuiz quiz =
  putQuizzesById quiz.quizId quiz
    |> Task.toResult
    |> Task.map UpdateQuizDone
    |> Effects.task


getQuizlets : QuizId -> Effects.Effects Action
getQuizlets quizId =
  getQuizzesByQuizIdQuizlets quizId
    |> Task.toResult
    |> Task.map GetQuizletsDone
    |> Effects.task
