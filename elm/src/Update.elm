module Update (..) where

import Models exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Routing
import Quiz.Update

update : Action -> AppModel -> (AppModel, Effects Action)
update action model =
  case (Debug.log "action" action) of
    RoutingAction routingAction ->
      let
        (updatedRouting, fx) =
          Routing.update routingAction model.routing
      in
        ( { model | routing = updatedRouting }
        , Effects.map RoutingAction fx
        )

    QuizAction quizAction ->
      let
        updatedModel =
          { quizzes = model.quizzes }

        (updatedQuizzes, fx) =
          Quiz.Update.update quizAction updatedModel
      in
        ( { model | quizzes = updatedQuizzes }
        , Effects.map QuizAction fx
        )

    NoOp ->
      (model, Effects.none)
