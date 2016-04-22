module Update (..) where

import Models exposing (..)
import Actions exposing (..)
import Mailboxes exposing (..)
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
        updatedModel = { quizzes = model.quizzes
                       , flashAddress = Signal.forwardTo
                                          actionsMailbox.address
                                          ShowFlashMessage
                       , confirmationAddress = confirmationsMailbox.address
                       }

        (updatedQuizzes, fx) =
          Quiz.Update.update quizAction updatedModel
      in
        ( { model | quizzes = updatedQuizzes }
        , Effects.map QuizAction fx
        )

    ShowFlashMessage message ->
      ( { model | flashMessage = message }, Effects.none )

    NoOp ->
      (model, Effects.none)
