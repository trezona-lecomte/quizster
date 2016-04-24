module Actions (..) where

import Quiz.Actions
import Quizlet.Actions
import Routing


type Action
  = NoOp
  | QuizAction Quiz.Actions.Action
  | QuizletAction Quizlet.Actions.Action
  | ShowFlashMessage String
  | RoutingAction Routing.Action
