module Quiz.Models (..) where

import API exposing (Quiz)


type alias QuizId =
  Int


new : Quiz
new =
  { quizId = 0
  , quizName = ""
  , quizDescription = ""
  }
