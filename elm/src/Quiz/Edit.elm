module Quiz.Edit (..) where

import Html exposing (..)
import Html.Attributes exposing (class, value, href, type', placeholder)
import Html.Events exposing (on, onClick, targetValue)
import API exposing (Quiz, Quizlet)
import Quiz.Navigation exposing (..)
import Quiz.Models exposing (..)
import Quiz.Actions exposing (..)


type alias ViewModel =
  { quiz : Quiz
  , quizlets : List Quizlet
  }


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
    , div
        []
        (List.map
          (quizletForm address)
          model.quizlets
        )
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


quizletForm : Signal.Address Action -> Quizlet -> Html.Html
quizletForm address quizlet =
  div
    []
    [ label [ class "label" ] [ text "Question" ]
    , p [ class "control" ] [ questionInput address quizlet ]
    , label [ class "label" ] [ text "Answer" ]
    , p [ class "control" ] [ answerInput address quizlet ]
    ]


questionInput : Signal.Address Action -> Quizlet -> Html.Html
questionInput address quizlet =
  textarea
    [ class "textarea"
    , placeholder "Enter a question..."
    , value quizlet.quizletQuestion
      -- , on
      --     "change"
      --     targetValue
      --     (\newQuestion ->
      --       Signal.message
      --         address
      --         (ChangeQuizletQuestion quizlet.quizletId newQuestion)
      --     )
    ]
    []


answerInput : Signal.Address Action -> Quizlet -> Html.Html
answerInput address quizlet =
  textarea
    [ class "textarea"
    , placeholder "Enter a answer..."
    , value quizlet.quizletAnswer
      -- , on
      --     "change"
      --     targetValue
      --     (\newAnswer ->
      --       Signal.message
      --         address
      --         (ChangeQuizletAnswer quizlet.quizletId newAnswer)
      --     )
    ]
    []


submitButton address model =
  button
    [ class "button is-primary"
    , onClick address GetQuizzes
    ]
    [ text "Done" ]
