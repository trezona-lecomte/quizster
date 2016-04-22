module Quiz.Update  (..) where

import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task
import API exposing (Quiz)
import Quiz.Actions exposing (..)
import Quiz.Models exposing (..)

type alias UpdateModel = { quizzes : List Quiz
                         , flashAddress : Signal.Address String
                         , confirmationAddress : Signal.Address (QuizId, String)
                         }

update : Action -> UpdateModel -> (List Quiz, Effects Action)
update action model =
  case action of
    ListQuizzes ->
      let
        path = "/quizzes/"
      in
        (model.quizzes, Effects.map HopAction (navigateTo path))

    GetQuizzesDone result ->
      case result of
        Ok quizzes ->
          (quizzes, Effects.none)
        Err error ->
          let
            flashMessage = toString error
            fx = Signal.send model.flashAddress flashMessage
                  |> Effects.task
                  |> Effects.map TaskDone
          in
            (model.quizzes, fx)

    CreateQuiz ->
      (model.quizzes, createQuiz new)

    CreateQuizDone result ->
      case result of
        Ok quiz ->
          let
            updatedQuizzes = quiz :: model.quizzes
            fx = Task.succeed (EditQuiz quiz.quizId)
                  |> Effects.task
          in
            (updatedQuizzes, fx)
        Err error ->
          let
            flashMessage = toString error
            fx = Signal.send model.flashAddress flashMessage
                  |> Effects.task
                  |> Effects.map TaskDone
          in
            (model.quizzes, fx)

    DeleteQuizIntent quiz ->
      let
        msg =
          "Are you sure you want to delete " ++ quiz.quizName ++ "?"
        fx =
          Signal.send model.confirmationAddress (quiz.quizId, msg)
            |> Effects.task
            |> Effects.map TaskDone
      in
        (model.quizzes, fx)

    DeleteQuiz quizId ->
      (model.quizzes, deleteQuiz quizId)

    DeleteQuizDone quizId result ->
      case result of
        Ok _ ->
          let
            notDeleted quiz =
              quiz.quizId /= quizId

            updatedQuizzes =
              List.filter notDeleted model.quizzes
          in
            (updatedQuizzes, Effects.none)
        Err error ->
          let
            message =
              toString error
            fx = Signal.send model.flashAddress message
                  |> Effects.task
                  |> Effects.map TaskDone
          in
            (model.quizzes, fx)

    UpdateQuiz quiz ->
      let
        changeQuiz q =
          if q.quizId /= quiz.quizId then
            { q | quizName = quiz.quizName, quizDescription = quiz.quizDescription }
          else
            q

        updatedQuizzes =
          List.map changeQuiz model.quizzes
      in
        (updatedQuizzes, Effects.none)

    SendQuizUpdate quiz ->
      let
        fxForQuiz q =
          if q.quizId /= quiz.quizId then
            Effects.none
          else
            updateQuiz quiz
        fx =
          List.map fxForQuiz model.quizzes
            |> Effects.batch
      in
        (model.quizzes, fx)

    UpdateQuizDone result ->
      case result of
        Ok quiz ->
          let
            updatedQuiz existing =
              if existing.quizId == quiz.quizId then
                quiz
              else
                existing

            updatedQuizzes =
              List.map updatedQuiz model.quizzes
          in
            (updatedQuizzes, Effects.none)

        Err error ->
          let
            message = toString error
            fx = Signal.send model.flashAddress message
                  |> Effects.task
                  |> Effects.map TaskDone
          in
            (model.quizzes, fx)

    EditQuiz id ->
      let
        path = "/quizzes/" ++ (toString id)
      in
        (model.quizzes, Effects.map HopAction (navigateTo path))

    TaskDone () ->
      (model.quizzes, Effects.none)

    HopAction _ ->
      (model.quizzes, Effects.none)

    NoOp ->
      (model.quizzes, Effects.none)
