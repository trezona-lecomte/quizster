module Models (..) where

import Routing
import API exposing (Quiz)

type alias AppModel = { quizzes : List Quiz
                      , routing : Routing.Model
                      }

initialModel : AppModel
initialModel = { quizzes = [ Quiz 0 "Test Quiz" "Boring!" ]
               , routing = Routing.initialModel
               }
