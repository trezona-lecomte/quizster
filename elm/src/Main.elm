module Main (..) where

import Html exposing (..)
import Effects exposing (Effects, Never)
import Task
import StartApp
import Routing
import API exposing (..)
import Actions exposing (..)
import Models exposing (..)
import Update exposing (..)
import Quiz.Actions exposing (getAllQuizzes)
import View exposing (..)


init : (AppModel, Effects Action)
init =
  let
    fxs = [ Effects.map QuizAction getAllQuizzes ]
    fx  = Effects.batch fxs
  in
    (Models.initialModel, fx)

app : StartApp.App AppModel
app =
  StartApp.start { init = init
                 , inputs = [ routerSignal ]
                 , update = update
                 , view = view
                 }

main : Signal.Signal Html
main =
  app.html

routerSignal : Signal Action
routerSignal =
  Signal.map RoutingAction Routing.signal

port runner : Signal (Task.Task Never ())
port runner =
  app.tasks

port routeRunTask : Task.Task () ()
port routeRunTask =
  Routing.run
