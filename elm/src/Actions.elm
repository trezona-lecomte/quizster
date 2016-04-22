module Actions (..) where

import Quiz.Actions
import Routing


type Action
  = NoOp
  | QuizAction Quiz.Actions.Action
  | ShowFlashMessage String
  | RoutingAction Routing.Action
