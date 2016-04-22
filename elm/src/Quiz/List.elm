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
    [ div [ class "left p2" ] [ text "Quizzes" ]
    , div [ class "right p1" ] [ addButton address model ]
    ]

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
    , td [] [ editButton address quiz
            , deleteButton address quiz
            ]
    ]

addButton : Signal.Address Action -> ViewModel -> Html.Html
addButton address model =
  button
    [ class "btn"
    , onClick address CreateQuiz
    ]
    [ i [ class "fa fa-plus mr1" ] []
    , text "New Quiz"
    ]

editButton : Signal.Address Action -> Quiz -> Html.Html
editButton address quiz =
  button
    [ class "btn regular"
    , onClick address (EditQuiz quiz.quizId)
    ]
    [ i [ class "fa fa-pencil mr1" ] []
    , text "Edit"
    ]

deleteButton : Signal.Address Action -> Quiz -> Html.Html
deleteButton address quiz =
  button
    [ class "btn regular mr1"
    , onClick address (DeleteQuizIntent quiz)
    ]
    [ i [ class "fa fa-trash mr1" ] []
    , text "Delete"
    ]
