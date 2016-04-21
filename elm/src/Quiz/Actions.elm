module Quiz.Actions (..) where

import Quiz.Models exposing (QuizId)
import Hop

type Action = NoOp
            | EditQuiz QuizId
            | ListQuizzes
            | HopAction ()
