module ReminderItem where

import Html exposing ( Html )
import Html.Attributes as A
import Time exposing ( Time )
import Result
--
import CustomTools exposing ((?), formatDate, parseDate)
import Static

-- ### Model ###

type alias Model = {
    body : String,
    created : Time,
    deadline : Maybe Time,
    overtime : Bool
}

init : Static.Reminder -> Model
init static =
    {   body = static.body
    ,   created = parseDate static.created
    ,   deadline = Nothing
    ,   overtime = False
    }

initNew : (String, String, Maybe String) -> Model
initNew (body, created, maybeDeadline) =
    {   body = body
    ,   created = parseDate created
    ,   deadline = Maybe.map parseDate maybeDeadline
    ,   overtime = False
    }

equal : Model -> Model -> Bool
equal r1 r2 = r1.body == r2.body
    && r1.created == r2.created
    && r1.deadline == r2.deadline

-- ### Update ###

type Action = ChangeTime Time

update : Action -> Model -> Model
update a model = case a of
    ChangeTime time -> case model.deadline of
        Just deadline -> { model | overtime = time > deadline}
        Nothing -> model

-- ### View ###

view : List Html -> Model -> Html
view deco model = Html.div [A.class ("item reminder")]
    [   Html.div [A.class "timestamp"]
            [   Html.text "Created: "
            ,   Html.text (formatDate model.created)]
    ,   Html.div [A.class ("timestamp" ++ (overtimeClass model))]
            [   Html.text "Deadline: "
            ,   Html.text ("None" ? Maybe.map formatDate model.deadline)]
    ,   Html.div [A.class "body" ] [Html.text model.body]
    ,   Html.div [A.class "buttons"] ([] ++ deco)
    ]

overtimeClass : Model -> String
overtimeClass model = if model.overtime then " overtime" else ""
