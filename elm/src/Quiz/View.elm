module Quiz.View (..) where

import Html exposing (..)
import Html.Attributes exposing (class, value, href)
import Html.Events exposing (onClick)
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
    , formName address model
    -- , formDescription address model
    ]

formName : Signal.Address Action -> ViewModel -> Html.Html
formName address model =
  div
    [ class "clearfix py1"
    ]
    [ div [ class "col col-5" ] [ text "Name" ]
    , div
        [ class "col col-7" ]
        [ input
            [ class "field-light"
            , value model.quiz.quizName
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
