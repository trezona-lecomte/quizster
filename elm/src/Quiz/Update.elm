module Quiz.Update  (..) where

import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import API exposing (Quiz)
import Quiz.Actions exposing (..)
import Quiz.Models exposing (..)

type alias UpdateModel = { quizzes : List Quiz }

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
          (model.quizzes, Effects.none)

    EditQuiz id ->
      let
        path = "/quizzes/" ++ (toString id)
      in
        (model.quizzes, Effects.map HopAction (navigateTo path))

    HopAction _ ->
      (model.quizzes, Effects.none)

    NoOp ->
      (model.quizzes, Effects.none)
