module Quiz.Edit (..) where

import Html exposing (..)
import Html.Attributes exposing (class, value, href, type', placeholder)
import Html.Events exposing (on, onClick, targetValue)
import API exposing (Quiz)
import Quiz.Navigation exposing (..)
import Quiz.Models exposing (..)
import Quiz.Actions exposing (..)


type alias ViewModel =
  { quiz : Quiz }


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
            [ form address model ]
        , div
            [ class "column is-1" ]
            []
        ]
    ]


form : Signal.Address Action -> ViewModel -> Html.Html
form address model =
  div
    []
    [ label [ class "label" ] [ text "Name" ]
    , p [ class "control" ] [ nameInput address model ]
    , label [ class "label" ] [ text "Description" ]
    , p [ class "control" ] [ descriptionInput address model ]
    , p [ class "control" ] [ submitButton address model ]
    ]


nameInput : Signal.Address Action -> ViewModel -> Html.Html
nameInput address model =
  input
    [ class "input"
    , type' "text"
    , placeholder "Enter a name..."
    , value model.quiz.quizName
    , on
        "change"
        targetValue
        (\newName ->
          Signal.message
            address
            (ChangeQuizName model.quiz.quizId newName)
        )
    ]
    []


descriptionInput : Signal.Address Action -> ViewModel -> Html.Html
descriptionInput address model =
  textarea
    [ class "textarea"
    , placeholder "Enter a description..."
    , value model.quiz.quizDescription
    , on
        "change"
        targetValue
        (\newDescription ->
          Signal.message
            address
            (ChangeQuizDescription model.quiz.quizId newDescription)
        )
    ]
    []


submitButton address model =
  button
    [ class "button is-primary"
    , onClick address ListQuizzes
    ]
    [ text "Done" ]



-- nameInput : Signal.Address Action -> ViewModel -> Html.Html
-- nameInput address model =
--   input
--     [ class "field-light"
--     , value model.quiz.quizName
--     , on
--         "change"
--         targetValue
--         (\newName ->
--           Signal.message
--             address
--             (ChangeQuizName model.quiz.quizId newName)
--         )
--     ]
--     []
-- inputDescription : Signal.Address Action -> ViewModel -> Html.Html
-- inputDescription address model =
--   input
--     [ class "field-light"
--     , value model.quiz.quizDescription
--     , on "change" targetValue
--         (\newDescription -> Signal.message address
--            (UpdateQuiz (Quiz model.quiz.quizId model.quiz.quizName newDescription))
--         )
--     ]
--     []
