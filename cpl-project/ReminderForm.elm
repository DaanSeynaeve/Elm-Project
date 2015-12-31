module ReminderForm where

import Html exposing (Html)
import List
import Signal
import Html.Attributes as A
import Html.Events as E
import Time exposing ( Time )
--
import CustomTools as CT exposing ((?))
import ReminderItem

-- ### MODEL ###

type alias Model = {
    body     : String,
    created  : Maybe String,
    deadline : Maybe String,
    time     : Time
}

init : Model
init =
    {   body = ""
    ,   created = Nothing
    ,   deadline = Nothing
    ,   time = 0
    }

defaultTime : Time -> String
defaultTime time = CT.formatDate time

-- ### Updates ###

type Action
    = Submit
    | Body String
    | Created String
    | Deadline String
    | ChangeTime Time

update : Action -> Model -> Model
update a model = case a of
    Body s       -> { model | body = s }
    Created s    -> { model | created = Just s }
    Deadline s   -> { model | deadline = Just s }
    Submit       -> { init  | time = model.time }
    ChangeTime t -> { model | time = t }

-- ### VIEW ###

view : Signal.Address Action -> Model -> Html
view address model =
    let message x = Signal.message (Signal.forwardTo address x)
    in Html.div [A.id "reminder-form"]
    [   CT.header "Add Reminder"
    ,   Html.textarea
            [   E.on "input" E.targetValue (message Body)
            ,   A.value model.body, A.rows 2
            ,   A.cols 40, A.class "big"
            ] []
    ,   Html.text "Date:"
    ,   viewDateInput ((defaultTime model.time) ? model.created) (message Created)
    ,   Html.text "Deadline:"
    ,   viewDateInput ("" ? model.deadline) (message Deadline)
    ,   Html.button [E.onClick address Submit] [Html.text "Add"]
    ]

viewDateInput : String -> (String -> Signal.Message) -> Html
viewDateInput s target = Html.input
    [   E.on "input" E.targetValue target
    ,   A.value s, A.type' "date"
    ] []

-- ### Result ###

makeResult : Action -> Model -> Maybe ReminderItem.Model
makeResult a model = case a of
    Submit -> let vals =
                (   model.body
                ,   ( defaultTime model.time) ? model.created
                ,   model.deadline)
              in Just (ReminderItem.initNew vals)
    _      -> Nothing
