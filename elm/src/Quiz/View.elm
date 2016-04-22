module Quiz.View (..) where

import Html exposing (..)
import Html.Attributes exposing (class, value, href)
import Html.Events exposing (on, onClick, targetValue)
import API exposing (Quiz)
import Quiz.Models exposing (..)
import Quiz.Actions exposing (..)


type alias ViewModel = { quiz : Quiz }

view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ nav address model
    , form address model
    ]

nav : Signal.Address Action -> ViewModel -> Html.Html
nav address model =
  div
    [ class "clearfix mb2 white bg-blue p1" ]
    [ listButton address model ]

form : Signal.Address Action -> ViewModel -> Html.Html
form address model =
  div
    [ class "m3" ]
    [ h1 [] [ text model.quiz.quizName ]
    , quizForm address model
    , updateButton address model
    ]

quizForm : Signal.Address Action -> ViewModel -> Html.Html
quizForm address model =
  div
    [ class "clearfix py1"
    ]
    [ div [ class "col col-3" ] [ text "Name" ]
    , div
        [ class "col col-3" ]
        [ input
            [ class "field-light"
            , value model.quiz.quizName
            ]
            []
        ]
    , div
        [ class "col col-6" ]
        [ input
            [ class "field-light"
            , value model.quiz.quizDescription
            ]
            []
        ]
    ]

listButton : Signal.Address Action -> ViewModel -> Html.Html
listButton address model =
  button
    [ class "btn regular"
    , onClick address ListQuizzes
    ]
    [ i [ class "fa fa-chevron-left mr1" ] []
    , text "All Quizzes"
    ]

inputName : Signal.Address Action -> ViewModel -> Html.Html
inputName address model =
  input
    [ class "field-light"
    , value model.quiz.quizName
    , on "change" targetValue
        (\newName -> Signal.message address
           (UpdateQuiz (Quiz model.quiz.quizId newName model.quiz.quizDescription))
        )
    ]
    []

inputDescription : Signal.Address Action -> ViewModel -> Html.Html
inputDescription address model =
  input
    [ class "field-light"
    , value model.quiz.quizDescription
    , on "change" targetValue
        (\newDescription -> Signal.message address
           (UpdateQuiz (Quiz model.quiz.quizId model.quiz.quizName newDescription))
        )
    ]
    []

updateButton : Signal.Address Action -> ViewModel -> Html.Html
updateButton address model =
  button
    [ class "btn btn regular"
    , onClick address (SendQuizUpdate model.quiz)
    ]
    [ i [ class "fa pencil mr1" ] []
    , text "Update Quiz"
    ]
