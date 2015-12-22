module ItemManager where

import Debug
import Html exposing ( Html )
import Signal exposing ( map )
import Html.Attributes as A exposing ( rel, href )
import Keyboard as K
--
import CustomTools exposing ( ($), watchSignal, isDefined, (?), toMaybe )
import ReminderItem
import List exposing ( (::) )
import ItemDecorator as ID exposing ( decorate )
import ItemFeed as Feed
import ReminderForm as Form

-- ### MODEL ###

type alias Model = (Feed.Model, Form.Model, Bool)

init : Feed.Model -> Form.Model -> Model
init feed form = (feed, form, True)

-- ### Actions ###

type Action = ToggleVisForm
            | FD Feed.Action
            | FM Form.Action
            | Transfer Feed.Action Form.Action

makeTransfer : ReminderItem.Model -> Form.Action -> Action
makeTransfer reminder a =
    let item = decorate ID.AReminder reminder
    in Transfer (Feed.AddItem item) a

update : Action -> Model -> Model
update action (feed, form, formvis) = case action of
    FD a -> (Feed.update a feed,form, formvis)
    FM b -> (feed, Form.update b form, formvis)
    Transfer ta tb -> (
        Feed.update ta feed,
        Form.update tb form,
        formvis )
    ToggleVisForm -> (feed, form, not formvis)

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address (feedState,formState,showForm) =
    let fm a = case Form.makeResult a formState of
        Just reminder -> makeTransfer reminder a
        Nothing -> FM a
    in Html.div [ A.id "pagewrap" ] [
        Feed.view
            (Signal.forwardTo address FD)
            feedState,
        if showForm
        then Form.view
            (Signal.forwardTo address fm)
            formState
        else Html.text ""
    ]
