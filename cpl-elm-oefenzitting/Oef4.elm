-- We implement a textbox that allows only digits.
-- The program below has a bug, it will be obvious when you play around with it.

-- The assignment is to use Elm's watch-expressions and fix the bug.
-- We provided 'watchSignal' to make it easier to watch entire signals.

import Debug

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A

import Char
import String

mailbox : Signal.Mailbox Model
mailbox = Signal.mailbox ""

type alias Model = String

init : Model
init = ""

-- UPDATE

type alias Action = String

update : Action -> Model -> Model
update action model =
  if String.all Char.isDigit action
  then action
  else model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address str =
  Html.input
        [ E.on "input" E.targetValue (Signal.message address)
        , A.value str
        , A.type' "text"]
        []

state : Signal Model
state = Signal.foldp update "" (watchSignal "boe" mailbox.signal)

main : Signal Html
main = Signal.map (view mailbox.address) state

watchSignal : String -> Signal a -> Signal a
watchSignal caption = Signal.map (Debug.watch caption)
