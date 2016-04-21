module Quiz.List  (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Quiz.Actions exposing (..)
import API exposing (Quiz)

type alias ViewModel = { quizzes : List Quiz }

type alias RenderHtml = Signal.Address Action -> ViewModel -> Html.Html

view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ nav address model
    , list address model
    ]

nav : Signal.Address Action -> ViewModel -> Html.Html
nav address model =
  div
    [ class "clearfix mb2 white bg-blue" ]
    [ div [ class "left p2" ] [ text "Quizzes" ] ]

list : Signal.Address Action -> ViewModel -> Html.Html
list address model =
  div
    []
    [ table
      [ class "table-light" ]
      [ thead
        []
        [ tr
          []
          [ th [] [ text "Id" ]
          , th [] [ text "Name" ]
          , th [] [ text "Description" ]
          , th [] [ text "High Score" ]
          ]
        ]
      , tbody
          []
          (List.map (quizRow address model) model.quizzes)
      ]
    ]

quizRow : Signal.Address Action -> ViewModel -> Quiz -> Html.Html
quizRow address model quiz =
  tr
    []
    [ td [] [ text (toString quiz.quizId) ]
    , td [] [ text quiz.quizName ]
    , td [] [ text quiz.quizDescription ]
    , td [] [ text (toString 9000) ]
    , td [] [ editButton address quiz ]
    ]

editButton : Signal.Address Action -> Quiz -> Html.Html
editButton address quiz =
  button
    [ class "btn regular"
    , onClick address (EditQuiz quiz.quizId)
    ]
    [ i [ class "fa fa-pencil mr1" ] [], text "Edit" ]
