module Quiz.Update (..) where

import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task
import API exposing (Quiz, Quizlet)
import Quiz.Actions exposing (..)
import Quiz.Models exposing (..)


type alias UpdateModel =
  { quizzes : List Quiz
  , quizlets : List Quizlet
  , flashAddress : Signal.Address String
  , confirmationAddress : Signal.Address ( QuizId, String )
  }


update : Action -> UpdateModel -> ( List Quiz, List Quizlet, Effects Action )
update action model =
  case action of
    GetQuizlets id ->
      let
        fx =
          getQuizlets id
      in
        ( model.quizzes, model.quizlets, fx )

    GetQuizletsDone result ->
      case result of
        Ok quizlets ->
          ( model.quizzes, quizlets, Effects.none )

        Err error ->
          let
            flashMessage =
              toString error

            fx =
              Signal.send model.flashAddress flashMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizzes, model.quizlets, fx )

    AttemptQuiz id ->
      let
        path =
          "/quizzes/" ++ (toString id) ++ "/attempt"
      in
        ( model.quizzes, model.quizlets, Effects.map HopAction (navigateTo path) )

    EditQuiz id ->
      let
        path =
          "/quizzes/" ++ (toString id)

        fx =
          Effects.batch
            [ getQuizlets id
            , Effects.map HopAction (navigateTo path)
            ]
      in
        ( model.quizzes, model.quizlets, fx )

    GetQuizzes ->
      let
        path =
          "/quizzes/"
      in
        ( model.quizzes, model.quizlets, Effects.map HopAction (navigateTo path) )

    GetQuizzesDone result ->
      case result of
        Ok quizzes ->
          ( quizzes, model.quizlets, Effects.none )

        Err error ->
          let
            flashMessage =
              toString error

            fx =
              Signal.send model.flashAddress flashMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizzes, model.quizlets, fx )

    CreateQuiz ->
      ( model.quizzes, model.quizlets, createQuiz new )

    CreateQuizDone result ->
      case result of
        Ok quiz ->
          let
            updatedQuizzes =
              quiz :: model.quizzes

            fx =
              Task.succeed (EditQuiz quiz.quizId)
                |> Effects.task
          in
            ( updatedQuizzes, model.quizlets, fx )

        Err error ->
          let
            flashMessage =
              toString error

            fx =
              Signal.send model.flashAddress flashMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizzes, model.quizlets, fx )

    DeleteQuizIntent quiz ->
      let
        msg =
          "Are you sure you want to delete " ++ quiz.quizName ++ "?"

        fx =
          Signal.send model.confirmationAddress ( quiz.quizId, msg )
            |> Effects.task
            |> Effects.map TaskDone
      in
        ( model.quizzes, model.quizlets, fx )

    DeleteQuiz quizId ->
      ( model.quizzes, model.quizlets, deleteQuiz quizId )

    DeleteQuizDone quizId result ->
      case result of
        Ok _ ->
          let
            notDeleted quiz =
              quiz.quizId /= quizId

            updatedQuizzes =
              List.filter notDeleted model.quizzes
          in
            ( updatedQuizzes, model.quizlets, Effects.none )

        Err error ->
          let
            message =
              toString error

            fx =
              Signal.send model.flashAddress message
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizzes, model.quizlets, fx )

    ChangeQuizName quizId newName ->
      let
        fxForQuiz quiz =
          if quiz.quizId /= quizId then
            Effects.none
          else
            let
              updatedQuiz =
                { quiz | quizName = newName }
            in
              updateQuiz updatedQuiz

        fx =
          List.map fxForQuiz model.quizzes
            |> Effects.batch
      in
        ( model.quizzes, model.quizlets, fx )

    ChangeQuizDescription quizId newDescription ->
      let
        fxForQuiz quiz =
          if quiz.quizId /= quizId then
            Effects.none
          else
            let
              updatedQuiz =
                { quiz | quizDescription = newDescription }
            in
              updateQuiz updatedQuiz

        fx =
          List.map fxForQuiz model.quizzes
            |> Effects.batch
      in
        ( model.quizzes, model.quizlets, fx )

    UpdateQuizDone result ->
      case result of
        Ok quiz ->
          let
            updatedQuiz existing =
              if existing.quizId == quiz.quizId then
                quiz
              else
                existing

            updatedCollection =
              List.map updatedQuiz model.quizzes
          in
            ( updatedCollection, model.quizlets, Effects.none )

        Err error ->
          let
            message =
              toString error

            fx =
              Signal.send model.flashAddress message
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizzes, model.quizlets, fx )

    TaskDone () ->
      ( model.quizzes, model.quizlets, Effects.none )

    HopAction _ ->
      ( model.quizzes, model.quizlets, Effects.none )

    NoOp ->
      ( model.quizzes, model.quizlets, Effects.none )
