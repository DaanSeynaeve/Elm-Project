module ItemManager where

import Debug
import Html exposing ( Html )
import Signal
import Html.Attributes as A
import Html.Events as E
import Keyboard as K
import Time exposing ( Time )
--
import CustomTools as CT exposing ( ($), watchSignal, isDefined, (?), toMaybe )
import ReminderItem
import List exposing ( (::) )
import ItemDecorator as ID exposing ( decorate )
import ItemFeed as Feed
import ReminderForm as Form

-- ### MODEL ###

type alias Model = {
    feed : Feed.Model,
    form : Form.Model,
    showForm : Bool,
    stylesheet : String
}

init : Feed.Model -> Form.Model -> Model
init feed form =
    {   feed = feed
    ,   form = form
    ,   showForm = True
    ,   stylesheet = "style.css"
    }

-- ### Actions ###

type Action
    = ToggleVisForm
    | FD Feed.Action
    | FM Form.Action
    | ChangeTime Time
    | ChangeStyle String
    | MultiAction (List Action)

update : Action -> Model -> Model
update action model = case action of
    MultiAction la -> List.foldl update model la
    FD a -> { model | feed = Feed.update a model.feed }
    FM a -> { model | form = Form.update a model.form }
    ToggleVisForm -> { model | showForm = not model.showForm }
    ChangeTime time -> update (makeTimeChange time model) model
    ChangeStyle stylesheet' -> { model | stylesheet = stylesheet'}

makeTransfer : ReminderItem.Model -> Form.Action -> Action
makeTransfer reminder a =
    let item = decorate ID.AReminder reminder
    in  MultiAction [FD (Feed.AddItem item), FM a]

makeTimeChange : Time -> Model -> Action
makeTimeChange time model =
    let changeItemTime = ID.ReminderAction (ReminderItem.ChangeTime time)
        a1 = FD (Feed.MapItemAction changeItemTime)
        a2 = FM (Form.ChangeTime time)
    in  MultiAction [a1, a2]

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address model =
    let fm a = case Form.makeResult a model.form of
        Just reminder -> makeTransfer reminder a
        Nothing -> FM a
    in Html.div [A.id "pagewrap"]
        [   Feed.view (Signal.forwardTo address FD) model.feed
        ,   if model.showForm
            then Form.view (Signal.forwardTo address fm) model.form
            else Html.text ""
        ,   viewConfig address model
        ]

viewConfig address state =
    Html.div []
        [   CT.header "Configuration"
        ,   Html.text "Style: "
        ,   let target = Signal.message (Signal.forwardTo address ChangeStyle)
            in Html.select [E.on "input" E.targetValue target]
                [   Html.option [A.value "style.css"] [Html.text "Default"]
                ,   Html.option [A.value ""] [Html.text "None"]
                ]
        ]
