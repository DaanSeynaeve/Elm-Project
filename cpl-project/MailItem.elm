module MailItem where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import String
import List exposing ( (::) )
--
import Static

-- # Model #

type alias Model = {
    static: Static.Email,
    collapsed: Bool
}

init : Static.Email -> Model
init m = { static = m, collapsed = True }

-- # Update #

type Action = ToggleCollapse
update : Action -> Model -> Model
update action model = case action of
    ToggleCollapse ->
        {   static = model.static,
            collapsed = not model.collapsed    }

-- ### View ###

view : Signal.Address Action -> Model -> List Html -> Html
view address state deco = Html.div [A.class "item email"] [
        Html.div [A.class "timestamp"]
            [Html.text "Received: ", Html.text (state.static.date)],
        Html.div [A.class "from"] [
            Html.text "From: ", Html.text (state.static.from)],
        Html.div [A.class "to"] [
            Html.text "To: ", Html.text (state.static.to)],
        Html.h4 [] [Html.text "Subj: ", Html.text (state.static.title)],
        Html.div [A.class "body"] [ Html.text (makeBody state) ],
        Html.div [A.class "buttons"] ((makeMoreButton address state)++deco)
    ]

makeBody : Model -> String
makeBody state = if state.collapsed
    then (String.fromList
        (List.take 200 (String.toList state.static.body)))
    else state.static.body

makeMoreButton : Signal.Address Action -> Model -> List Html
makeMoreButton address state = let lbl = case state.collapsed of
        True -> "More"
        False -> "Less"
    in if String.length state.static.body > 200
    then [Html.button [E.onClick address ToggleCollapse] [Html.text lbl]]
    else []
