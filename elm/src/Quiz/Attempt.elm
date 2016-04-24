module Quiz.Attempt (..) where

import Html exposing (..)
import Html.Attributes exposing (class, value, href, type', placeholder)
import Html.Events exposing (on, onClick, targetValue)
import API exposing (Quiz, Quizlet)
import Quiz.Navigation exposing (..)
import Quiz.Models exposing (..)
import Actions exposing (..)
import Quizlet.View


type alias ViewModel =
  { quiz : Quiz
  , quizlets : List Quizlet
  }


view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ navbar (Signal.forwardTo address QuizAction)
    , div
        [ class "columns" ]
        [ div
            [ class "column is-1" ]
            []
        , div
            [ class "column is-10" ]
            [ Quizlet.View.view
                (Signal.forwardTo address QuizletAction)
                { quizlet = (currentQuizlet model) }
            ]
        , div
            [ class "column is-1" ]
            []
        ]
    ]


currentQuizlet : ViewModel -> Quizlet
currentQuizlet model =
  let
    maybeQuizlet =
      List.head model.quizlets
  in
    case maybeQuizlet of
      Just quizlet ->
        quizlet

      Nothing ->
        Quizlet 0 model.quiz.quizId "" ""
