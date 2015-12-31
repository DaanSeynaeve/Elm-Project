module ItemDecorator where

import Signal
import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import List
import Time exposing (Time)
--
import MailItem
import ReminderItem

-- ### Model ###

type Item = AMail MailItem.Model | AReminder ReminderItem.Model

type alias Model = {
    item: Item,
    pinned: Bool,
    done: Bool
}

decorate : (m -> Item) -> m -> Model
decorate t m = {item=(t m),pinned=False,done=False}

itemTime : Model -> Time
itemTime model = case model.item of
    AMail item -> item.date
    AReminder item -> item.created

itemEqual : Model -> Model -> Bool
itemEqual model1 model2 = case (model1.item,model2.item) of
    (AMail m1, AMail m2) -> MailItem.equal m1 m2
    (AReminder r1, AReminder r2) -> ReminderItem.equal r1 r2
    _ -> False

-- ### Update ###

type Action
    = TogglePinned
    | ToggleDone
    | MailAction MailItem.Action
    | ReminderAction ReminderItem.Action

update : Action -> Model -> Model
update a model = case (a,model.item) of
    (TogglePinned,_) -> { model | pinned = not model.pinned }
    (ToggleDone,_) -> { model | done = not model.done }
    (MailAction ma, AMail m) ->
        { model | item = AMail (MailItem.update ma m) }
    (ReminderAction ra, AReminder r) ->
        { model | item = AReminder (ReminderItem.update ra r) }
    _ -> model

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address model =
    let deco = (makeDecoration address model)
        decoratedView = case model.item of
            AMail item -> MailItem.view deco (Signal.forwardTo address MailAction) item
            AReminder item -> ReminderItem.view deco item
    in Html.div
        [A.class (if model.pinned then "pinned" else "")]
        [decoratedView]

makeDecoration : Signal.Address Action -> Model -> List Html
makeDecoration address model =
    [   Html.button [E.onClick address TogglePinned] [lblPinned model]
    ,   Html.button [E.onClick address ToggleDone] [lblDone model]
    ]

lblDone state = Html.text (if state.done then "Undo" else "Done")
lblPinned state = Html.text (if state.pinned then "Unpin" else "Pin")
