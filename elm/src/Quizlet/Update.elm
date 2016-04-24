module Quizlet.Update (..) where

import Effects exposing (Effects)
import Hop.Navigate exposing (navigateTo)
import Task
import API exposing (Quizlet)
import Actions
import Quizlet.Actions exposing (..)


type alias UpdateModel =
  { quizlets : List Quizlet
  , flashAddress : Signal.Address String
  }


update : Action -> UpdateModel -> ( List Quizlet, Effects Action )
update action model =
  case action of
    GetQuizlets id ->
      let
        path =
          "/quizzes/" ++ (toString id) ++ "/quizlets"

        fx =
          Effects.map HopAction (navigateTo path)

        -- , Effects.map Actions.QuizletAction (getAllQuizlets id)
      in
        ( model.quizlets, fx )

    GetQuizletsDone result ->
      case result of
        Ok quizlets ->
          ( quizlets, Effects.none )

        Err error ->
          let
            flashMessage =
              toString error

            fx =
              Signal.send model.flashAddress flashMessage
                |> Effects.task
                |> Effects.map TaskDone
          in
            ( model.quizlets, fx )

    SubmitAnswer id answer ->
      ( model.quizlets, Effects.none )

    HopAction () ->
      ( model.quizlets, Effects.none )

    TaskDone () ->
      ( model.quizlets, Effects.none )

    NoOp ->
      ( model.quizlets, Effects.none )
