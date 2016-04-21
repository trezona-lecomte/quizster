module Models (..) where

import Routing
import API exposing (Quiz)

type alias AppModel = { quizzes : List Quiz
                      , routing : Routing.Model
                      }

initialModel : AppModel
initialModel = { quizzes = []
               , routing = Routing.initialModel
               }
