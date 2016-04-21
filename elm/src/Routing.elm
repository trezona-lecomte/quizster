module Routing (..) where

import Task exposing (Task)
import Effects exposing (Effects, Never)
import Hop
import Hop.Types exposing (Location, PathMatcher, Router, newLocation)
import Hop.Navigate exposing (navigateTo)
import Hop.Matchers exposing (match1, match2, match3, int)
import Quiz.Models exposing (QuizId)


type Route = QuizzesRoute
           | QuizRoute QuizId
           | NotFoundRoute


{- Routing Actions:
HopAction:
To change the browser location Hop returns a task that need to be
run by a port.  We need to wrap this task in an action, but the
result of the task is not important, thus the () payload.

ApplyRoute:
When the browser location changes Hop will match the path with the
matchers (defined below) Hop provides a signal of (Route,
Location). Where Route our matched route and Location is a record
with information about the current location.

NavigateTo:
Action to start a browser location change. -}

type Action = HopAction ()
            | ApplyRoute (Route, Location)
            | NavigateTo String


{- Model:
We need to store the current location given by Hop. This location is
needed for some navigation and for extracting the current query.  We
also store the current route so we can display the correct views. -}

type alias Model = { location : Location
                   , route : Route
                   }

initialModel : Model
initialModel = { location = newLocation
               , route = QuizzesRoute
               }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    {- NavigateTo:
    Called from our views, e.g. by clicking on a button.
    Asks Hop to change the page location. -}
    NavigateTo path ->
      (model, Effects.map HopAction (navigateTo path))

    {- ApplyRoute:
    When we get a new value from the hop signal, this
    action is triggered.
    Here we store the current route & location in the model. -}
    ApplyRoute (route, location) ->
      ({ model | route = route, location = location }, Effects.none)

    HopAction () ->
      (model, Effects.none)


{- Route Matchers: These are in charge of matching the browser path to
our routes defined above, e.g. "/quizzes" --> QuizzesRoute -}

indexMatcher : PathMatcher Route
indexMatcher =
  match1 QuizzesRoute "/"

quizzesMatcher : PathMatcher Route
quizzesMatcher =
  match1 QuizzesRoute "/quizzes"

quizMatcher : PathMatcher Route
quizMatcher =
  match2 QuizRoute "/quizzes/" int

matchers : List (PathMatcher Route)
matchers =
  [ indexMatcher
  , quizzesMatcher
  , quizMatcher
  ]


{- Hop Router: Hope expects a list of matchers and one Route for when
there are no matches. -}

router : Router Route
router =
  Hop.new { matchers = matchers
          , notFound = NotFoundRoute
          }


{- In order to change the location on the browser when we first run
the application, we need to send a task to a port to do it. -}

run : Task () ()
run = router.run


{- Hop provides a signal that updates when the browser location
changes.  This signal has the type (Route, Location).  Here we map
this signal to one our application can use i.e. ApplyRoute (Route,
Location) -}

signal : Signal Action
signal = Signal.map ApplyRoute router.signal
