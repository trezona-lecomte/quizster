module Quiz.Navigation (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Quiz.Actions exposing (..)


navbar : Signal.Address Action -> Html.Html
navbar address =
  nav
    [ class "navbar", style [ ( "min-height", "70px" ) ] ]
    [ p
        [ class "navbar-item has-text-centered" ]
        [ a
            [ class "link"
            , onClick address ListQuizzes
            ]
            [ text "Home" ]
        ]
    , p
        [ class "navbar-item has-text-centered" ]
        [ a
            [ class "link"
            , onClick address ListQuizzes
            ]
            [ text "Menu" ]
        ]
    , p
        [ class "navbar-item has-text-centered" ]
        [ img
            [ alt ""
            , src "/img/QuizsterLogo.png"
            ]
            []
        ]
    , p
        [ class "navbar-item has-text-centered" ]
        [ a
            [ class "link"
            , onClick address CreateQuiz
            ]
            [ text "Create Quiz" ]
        ]
    , p
        [ class "navbar-item has-text-centered" ]
        [ a
            [ class "link"
            , onClick address ListQuizzes
            ]
            [ text "Contact" ]
        ]
    ]
