module MailItem where

import Html exposing ( Html )
import Html.Events as E
import Html.Attributes as A
import String
import List exposing ( (::) )
import CustomTools exposing (formatDate, parseDate)
import Time exposing (Time)
--
import Static

-- ### Model ###

type alias Model = {
    date : Time,
    from : String,
    to : String,
    title : String,
    body : String,
    collapsed: Bool
}

init : Static.Email -> Model
init static =
    {   date = parseDate static.date
    ,   from = static.from
    ,   to = static.to
    ,   title = static.title
    ,   body = static.body
    ,   collapsed = True
    }

tooLong : Model -> Bool
tooLong model = (String.length model.body) > 200

equal : Model -> Model -> Bool
equal m1 m2 = m1.date == m2.date
    && m1.from == m2.from
    && m1.to == m2.to
    && m1.title == m2.title
    && m1.body == m2.body

-- ### Update ###

type Action = ToggleCollapse

update : Action -> Model -> Model
update action model = case action of
    ToggleCollapse -> { model | collapsed = not model.collapsed }

-- ### View ###

view : List Html -> Signal.Address Action -> Model -> Html
view deco address model = Html.div [A.class "item email"]
    [   Html.div [A.class "timestamp"]
            [Html.text "Received: ", Html.text (formatDate model.date)]
    ,   Html.div [A.class "from"] [
            Html.text "From: ", Html.text (model.from)]
    ,   Html.div [A.class "to"] [
            Html.text "To: ", Html.text (model.to)]
    ,   Html.h4 [] [Html.text "Subj: ", Html.text (model.title)]
    ,   Html.div [A.class "body"] [ Html.text (makeBody model) ]
    ,   Html.div [A.class "buttons"] ((makeMoreButton address model)++deco)
    ]

makeBody : Model -> String
makeBody model =
    if model.collapsed && tooLong model
    then (String.left 200 model.body) ++ "..."
    else model.body

makeMoreButton : Signal.Address Action -> Model -> List Html
makeMoreButton address model =
    let lbl = if model.collapsed then "More" else "Less"
    in  if tooLong model
        then [Html.button [E.onClick address ToggleCollapse] [Html.text lbl]]
        else []
