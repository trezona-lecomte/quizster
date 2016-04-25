module Update (..) where

import Models exposing (..)
import Actions exposing (..)
import Mailboxes exposing (..)
import Effects exposing (Effects)
import Routing
import Quiz.Update
import Quizlet.Update


update : Action -> AppModel -> ( AppModel, Effects Action )
update action model =
  case (Debug.log "action" action) of
    RoutingAction routingAction ->
      let
        ( updatedRouting, fx ) =
          Routing.update routingAction model.routing
      in
        ( { model | routing = updatedRouting }
        , Effects.map RoutingAction fx
        )

    QuizAction quizAction ->
      let
        updatedModel =
          { quizzes = model.quizzes
          , quizlets = model.quizlets
          , flashAddress =
              Signal.forwardTo
                actionsMailbox.address
                ShowFlashMessage
          , confirmationAddress = confirmationsMailbox.address
          }

        ( updatedQuizzes, updatedQuizlets, fx ) =
          Quiz.Update.update quizAction updatedModel
      in
        ( { model | quizzes = updatedQuizzes, quizlets = updatedQuizlets }
        , Effects.map QuizAction fx
        )

    QuizletAction quizletAction ->
      let
        updatedModel =
          { quizlets = model.quizlets
          , flashAddress =
              Signal.forwardTo
                actionsMailbox.address
                ShowFlashMessage
          }

        ( updatedQuizlets, fx ) =
          Quizlet.Update.update quizletAction updatedModel
      in
        ( { model | quizlets = updatedQuizlets }
        , Effects.map QuizletAction fx
        )

    ShowFlashMessage message ->
      ( { model | flashMessage = message }, Effects.none )

    NoOp ->
      ( model, Effects.none )
