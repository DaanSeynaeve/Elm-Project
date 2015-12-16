module Counter where

import Signal
import Html exposing ( Html )
import Html.Events as E

type alias Model = Int

init : Model
init = 0

-- UPDATE


type Action = Increment
update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1

-- VIEW

view : Signal.Address Action -> Model -> Html
view address int =
  Html.button
        [ E.onClick address Increment ]
        [ Html.text <| toString int ]
