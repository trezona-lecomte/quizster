module View (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Actions exposing (..)
import Models exposing (..)
import Routing
import Quiz.Models exposing (QuizId)
import Quiz.List exposing (view)
import Quiz.Edit
import Quiz.Attempt


view : Signal.Address Action -> AppModel -> Html
view address model =
  -- let
  -- _ =
  --   Debug.log "model" model
  -- in
  div
    [ class "" ]
    [ div
        [ class "" ]
        []
    , div
        [ class "" ]
        [ flash address model
        , page address model
        ]
    , div
        [ class "" ]
        []
    ]


flash address model =
  if String.isEmpty model.flashMessage then
    span [] []
  else
    div
      [ class "bold center p2 mb2 white bg-red rounded" ]
      [ text model.flashMessage ]


page : Signal.Address Action -> AppModel -> Html.Html
page address model =
  case model.routing.route of
    Routing.QuizzesRoute ->
      quizzesPage address model

    Routing.QuizRoute quizId ->
      quizPage address model quizId

    Routing.AttemptQuizRoute quizId ->
      attemptQuizPage address model quizId

    Routing.NotFoundRoute ->
      notFoundView


quizzesPage : Signal.Address Action -> AppModel -> Html.Html
quizzesPage address model =
  let
    viewModel =
      { quizzes = model.quizzes }
  in
    Quiz.List.view (Signal.forwardTo address QuizAction) viewModel


quizPage : Signal.Address Action -> AppModel -> QuizId -> Html.Html
quizPage address model quizId =
  let
    maybeQuiz =
      model.quizzes
        |> List.filter (\quiz -> quiz.quizId == quizId)
        |> List.head
  in
    case maybeQuiz of
      Just quiz ->
        let
          viewModel =
            { quiz = quiz
            , quizlets = model.quizlets
            }
        in
          Quiz.Edit.view (Signal.forwardTo address QuizAction) viewModel

      Nothing ->
        notFoundView


attemptQuizPage : Signal.Address Action -> AppModel -> QuizId -> Html.Html
attemptQuizPage address model quizId =
  let
    maybeQuiz =
      model.quizzes
        |> List.filter (\quiz -> quiz.quizId == quizId)
        |> List.head
  in
    case maybeQuiz of
      Just quiz ->
        let
          viewModel =
            { quiz = quiz
            , quizlets = model.quizlets
            }
        in
          Quiz.Attempt.view address viewModel

      Nothing ->
        notFoundView


notFoundView : Html.Html
notFoundView =
  div
    []
    [ text "Not Found"
    ]
