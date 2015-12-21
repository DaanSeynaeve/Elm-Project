module ItemManager where

import Debug
import Html exposing ( Html )
import Signal exposing ( map )
import Html.Attributes as A exposing ( rel, href )
import Keyboard as K
--
import CustomTools exposing ( ($), watchSignal, isDefined, (?), toMaybe )
-- import MailItem
-- import ReminderItem
-- import Static
import List exposing ( (::) )
-- import ItemDecorator as ID exposing ( decorate )
import ItemFeed as Feed
import ReminderForm as Form

-- ### MODEL ###

type alias Model = (Feed.Model, Form.Model, Bool)

init : Feed.Model -> Form.Model -> Model
init feed form = (feed, form, True)

-- ### Actions ###

type Action =
    ToggleVisForm
    | FD Feed.Action
    | FM Form.LocalAction
    | Transfer Feed.Action Form.LocalAction
--    | List Action

update : Action -> Model -> Model
update action (feed, form, formvis) = case action of
    FD a -> (Feed.update a feed,form, formvis)
    FM b -> (feed, Form.update b form, formvis)
    Transfer ta tb -> (
        Feed.update ta feed,
        Form.update tb form,
        formvis )
    ToggleVisForm -> (feed, form, not formvis)
--    _ -> (feed,form,formvis)

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address (feedState,formState,showForm) =
    let transfertag x = Transfer (Feed.AddItem (fst x)) (snd x)
    in Html.div [ A.id "pagewrap" ] [
        Feed.view
            (Signal.forwardTo address FD)
            feedState,
        if showForm
        then Form.view
            (Signal.forwardTo address transfertag)
            formState
        else Html.text ""
    ]
