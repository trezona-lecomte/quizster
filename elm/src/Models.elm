module Models (..) where

import Routing
import API exposing (Quiz, Quizlet)


type alias AppModel =
  { quizzes : List Quiz
  , quizlets : List Quizlet
  , routing : Routing.Model
  , flashMessage : String
  }


initialModel : AppModel
initialModel =
  { quizzes = []
  , quizlets = []
  , routing = Routing.initialModel
  , flashMessage = ""
  }
