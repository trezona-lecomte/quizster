module Quizlet.List (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import API exposing (Quizlet)
import Quiz.Navigation exposing (..)
import Quizlet.Actions exposing (..)


-- [{ quizletId = 1, quizletQuizId = 1, quizletQuestion = "What is the greatest text editor of all time?", quizletAnswer = "Emacs" }]


type alias ViewModel =
  { quizlets : List Quizlet }


view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ navbar address
    , div
        [ class "columns" ]
        [ div
            [ class "column is-1" ]
            []
        , div
            [ class "column is-10" ]
            [ list address model ]
        , div
            [ class "column is-1" ]
            []
        ]
    ]


list : Signal.Address Action -> ViewModel -> Html.Html
list address model =
  div [] []
