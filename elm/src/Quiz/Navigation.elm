module Quiz.Navigation (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Quiz.Actions exposing (..)


navbar : Signal.Address Action -> Html.Html
navbar address =
  nav
    []
    [ div
        [ class "nav-wrapper" ]
        [ a
            [ class "brand-logo", href "#!" ]
            [ text "Logo" ]
        , a
            [ class "button-collapse", attribute "data-activates" "mobile-demo", href "#" ]
            [ i
                [ class "material-icons" ]
                [ text "menu" ]
            ]
        , ul
            [ class "right hide-on-med-and-down" ]
            [ li
                []
                [ a
                    [ href "sass.html" ]
                    [ text "Sass" ]
                ]
            ]
        , ul
            [ class "side-nav", id "mobile-demo" ]
            [ li
                []
                [ a
                    [ href "sass.html" ]
                    [ text "Sass" ]
                ]
            ]
        ]
    ]



-- nav
--   [ class "blue-grey darken-1"
--   , attribute "role" "navigation"
--   ]
--   [ div
--       [ class "nav-wrapper container" ]
--       [ mainHeading address
--       , ul
--           [ class "right hide-on-med-and-down" ]
--           [ li [] [ text "Navbar link" ] ]
--       , ul
--           [ id "nav-mobile"
--           , class "side-nav"
--           , style [ ( "transform", "translateX(-100%)" ) ]
--           ]
--           [ li [] [ text "Navbar link" ] ]
--       , a
--           [ href "#"
--           , attribute "data-activates" "nav-mobile"
--           , class "button-collapse"
--           ]
--           [ i [ class "material-icons" ] [ text "menu" ] ]
--         -- , homeLink address
--       , div
--           [ class "navbar-right" ]
--           [ quizListLink address
--           ]
--       ]
--   ]


mainHeading : Signal.Address Action -> Html.Html
mainHeading address =
  a
    [ class "brand-logo"
    , onClick address ListQuizzes
    ]
    [ text "Quizster" ]


homeLink : Signal.Address Action -> Html.Html
homeLink address =
  p
    [ class "navbar-item"
    ]
    [ a [ onClick address ListQuizzes ] [ text "Home" ]
    ]


quizListLink : Signal.Address Action -> Html.Html
quizListLink address =
  p
    [ class "navbar-item"
    ]
    [ a
        [ onClick address ListQuizzes ]
        [ text "All Quizzes" ]
    ]
