module ReminderForm where

import Html exposing (Html)
import List
import Signal
import Html.Attributes as A
import Html.Events as E
import Time exposing ( Time, every, second, hour )
import Date exposing ( Date, toTime, fromTime, day )
--
import CustomTools as CT exposing ((?))
import ReminderItem

-- ### Outgoing signal ###

-- TODO : maybe do this better with port task?
signal : Signal Action
signal = Signal.map ChangeTime <| first (every second)

first : Signal Time -> Signal Time
first time = Signal.dropRepeats <|
    Signal.foldp (\x y -> if y == 0 then x else min x y) 0 time

-- ### MODEL ###

type alias Model = {
    body     : String,
    created  : Maybe String,
    deadline : String,
    time     : Time
}

init : Model
init = {
        body = "",
        created = Nothing,
        deadline = "",
        time = 0
    }

defaultTime : Time -> String
defaultTime time = CT.formatDate (fromTime time)

-- ### Updates ###

type Action = Submit
            | Body String
            | Created String
            | Deadline String
            | ChangeTime Time

update : Action -> Model -> Model
update a model = case a of
    Body s       -> { model | body = s }
    Created s    -> { model | created = Just s }
    Deadline s   -> { model | deadline = s }
    Submit       -> { init  | time = model.time }
    ChangeTime t -> { model | time = t }

-- ### VIEW ###

view : Signal.Address Action -> Model -> Html
view address state =
    let tag x = Signal.message (Signal.forwardTo address x)
    in Html.div [A.id "reminder-form"] [
        CT.header "Add Reminder",
        Html.textarea
            [   E.on "input" E.targetValue (tag Body)
            ,   A.value state.body, A.rows 2
            ,   A.cols 40, A.class "big"
            ] [],
        viewDateInput ((defaultTime state.time) ? state.created) (tag Created),
        viewDateInput state.deadline (tag Deadline),
        Html.button
            [E.onClick address Submit]
            [Html.text "Add"]
    ]

viewDateInput : String -> (String -> Signal.Message) -> Html
viewDateInput s target =
    Html.input [   E.on "input" E.targetValue target
               ,   A.value s, A.type' "date"
               ] []

-- ### Result ###

makeResult : Action -> Model -> Maybe ReminderItem.Model
makeResult a model = case a of
    Submit -> let vals = ( model.body
                         , ( defaultTime model.time) ? model.created
                         , model.deadline)
              in Just (ReminderItem.initNew vals)
    _      -> Nothing
