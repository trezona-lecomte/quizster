module Quiz.List (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import API exposing (Quiz)
import Quiz.Navigation exposing (..)
import Quiz.Actions exposing (..)


type alias ViewModel =
  { quizzes : List Quiz }


type alias RenderHtml =
  Signal.Address Action -> ViewModel -> Html.Html


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
  div
    [ class "column" ]
    [ table
        [ class "table" ]
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
    , td [ class "is-link" ] [ showButton address quiz ]
    , td [ class "is-link is-icon" ] [ editButton address quiz ]
    , td [ class "is-link is-icon" ] [ deleteButton address quiz ]
    ]


showButton : Signal.Address Action -> Quiz -> Html.Html
showButton address quiz =
  a
    [ onClick address (AttemptQuiz quiz.quizId) ]
    [ p [] [ text "Take Quiz!" ] ]


editButton : Signal.Address Action -> Quiz -> Html.Html
editButton address quiz =
  a
    [ onClick address (EditQuiz quiz.quizId) ]
    [ i [ class "fa fa-pencil" ] []
    ]


deleteButton : Signal.Address Action -> Quiz -> Html.Html
deleteButton address quiz =
  a
    [ onClick address (DeleteQuizIntent quiz) ]
    [ i [ class "fa fa-trash" ] []
    ]
