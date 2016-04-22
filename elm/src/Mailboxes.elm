module Mailboxes (..) where

import Actions exposing (..)


actionsMailbox : Signal.Mailbox Action
actionsMailbox =
  Signal.mailbox NoOp


confirmationsMailbox : Signal.Mailbox ( Int, String )
confirmationsMailbox =
  Signal.mailbox ( 0, "" )
