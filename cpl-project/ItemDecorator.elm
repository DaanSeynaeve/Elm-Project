module ItemDecorator where

import Signal
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import List
import Date
--
import MailItem
import ReminderItem

-- # Model #

type Item = AMail MailItem.Model | AReminder ReminderItem.Model
type alias Model = {
        item: Item,
        pinned: Bool,
        done: Bool
    }

-- t must be AMail or AReminder
decorate : (m -> Item) -> m -> Model
decorate t m = {item=(t m),pinned=False,done=False}

dateString : Model -> String
dateString model = case model.item of
    AMail item -> item.static.date
    AReminder item -> item.static.created

-- # Update #

type Action = TogglePinned | ToggleDone | MailAction MailItem.Action

update : Action -> Model -> Model
update action {item,pinned,done} = let (i,p,d) = case action of
        TogglePinned -> (item,not(pinned),done)
        ToggleDone -> (item,pinned,not(done))
        MailAction ma -> case item of
            AMail m -> (AMail (MailItem.update ma m), pinned, done)
            AReminder r -> (item,pinned,done)
        _ -> (item,pinned,done)
    in {item = i, pinned = p, done = d}

-- # View #

view : Signal.Address Action -> Model -> Html
view address state = let deco = (makeDecoration address state) in
    let innerHtml = case state.item of
        AMail item -> MailItem.view (Signal.forwardTo address MailAction) item deco
        AReminder item -> ReminderItem.view item deco
    in Html.div [A.class (if state.pinned then "pinned" else "")] [innerHtml]

makeDecoration : Signal.Address Action -> Model -> List Html
makeDecoration address state = [
        Html.button [E.onClick address TogglePinned] [lblPinned state],
        Html.button [E.onClick address ToggleDone] [lblDone state]
    ]

lblDone state = Html.text (if state.done then "Undo" else "Done")
lblPinned state = Html.text (if state.pinned then "Unpin" else "Pin")
