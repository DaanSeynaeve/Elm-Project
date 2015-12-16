module ReminderItem where

import Html exposing ( Html )
import Html.Attributes as A
--
import Static

type alias Model = {
    static : Static.Reminder,
    deadline : String
}

init : Static.Reminder -> Model
init static = {static=static, deadline=""}

initNew : (String, String, String) -> Model
initNew (body,created,deadline) =
    {static={body=body,created=created}, deadline=deadline}

view : Model -> List Html -> Html
view model deco = Html.div [A.class "item reminder"] [
        Html.div [A.class "timestamp"]
            [Html.text "Created: ", Html.text model.static.created],
        Html.div [A.class "timestamp"]
            [Html.text "Deadline: ", Html.text model.deadline],
        Html.div [A.class "body" ] [Html.text model.static.body],
        Html.div [A.class "buttons"] ([] ++ deco)
    ]
