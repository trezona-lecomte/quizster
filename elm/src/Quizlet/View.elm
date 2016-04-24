module Quizlet.View (..) where

import Html exposing (..)
import API exposing (..)
import Quizlet.Models exposing (..)
import Quizlet.Actions exposing (..)


type alias ViewModel =
  { quizlet : Quizlet }


view : Signal.Address Action -> ViewModel -> Html.Html
view address model =
  div
    []
    [ text model.quizlet.quizletQuestion ]
