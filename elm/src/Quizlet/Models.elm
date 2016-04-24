module Quizlet.Models (..) where

import API exposing (Quizlet)


type alias QuizletId =
  Int


new : Quizlet
new =
  { quizletId = 0
  , quizletQuizId = 0
  , quizletQuestion = ""
  , quizletAnswer = ""
  }
