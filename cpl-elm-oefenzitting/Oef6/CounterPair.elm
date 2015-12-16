module CounterPair where

import Signal
import Html exposing ( Html )
import Counter

type alias Model = (Counter.Model, Counter.Model)

init : Model
init = (Counter.init, Counter.init)

-- UPDATE

type Action = Left Counter.Action
            | Right Counter.Action

update : Action -> Model -> Model
update action (left, right) =
  case action of
    Left subAction -> (Counter.update subAction left, right)
    Right subAction -> (left, Counter.update subAction right)

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div []
      [ Counter.view (Signal.forwardTo address Left) (fst model)
      , Counter.view (Signal.forwardTo address Right) (snd model) ]
