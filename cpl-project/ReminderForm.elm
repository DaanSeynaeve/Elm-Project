module ReminderForm where

import Html exposing (Html)
import List
import Signal
import Html.Attributes as A
import Html.Events as E
--
import Static
import CustomTools as CT
import ItemDecorator as ID
import ReminderItem

-- ### Outgoing signal ###

signal : Signal LocalAction
signal = localbox.signal

-- ### MODEL ###

type alias Model = (String, String, String)

init : Model
init = ("","","")

makeReminder : Model -> ID.Model
makeReminder model =
    ID.decorate ID.AReminder (ReminderItem.initNew model)

-- ### LOCAL MAILBOX ###

type alias Submit = (ID.Model, LocalAction)

type LocalAction = Clear
    | Body String
    | Created String
    | Deadline String

localbox : Signal.Mailbox LocalAction
localbox = Signal.mailbox Clear

update : LocalAction -> Model -> Model
update a (body,created,deadline) = case a of
    Body s      -> (s,created,deadline)
    Created s   -> (body,s,deadline)
    Deadline s  -> (body,created,s)
    Clear       -> init

-- ### VIEW ###

view : Signal.Address Submit -> Model -> Html
view address (b,c,d) = let model = (b,c,d) in
    let tag x = Signal.message (Signal.forwardTo localbox.address x)
    in Html.div [A.id "reminder-form"] [
        CT.header "Add Reminder",
        Html.textarea
            [   E.on "input" E.targetValue (tag Body),
                A.value b, A.rows 2,
                A.cols 40, A.class "big"   ] [],
        Html.input
            [   E.on "input" E.targetValue (tag Created),
                A.value c, A.type' "date"  ] [],
        Html.input
            [   E.on "input" E.targetValue (tag Deadline),
                A.value d, A.type' "date"] [],
        Html.button
            [E.onClick address (makeReminder model, Clear)]
            [Html.text "Add"]
    ]
