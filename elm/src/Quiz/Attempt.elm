module Quiz.Attempt (..) where

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
            [ text "content here" ]
        , div
            [ class "column is-1" ]
            []
        ]
    ]
