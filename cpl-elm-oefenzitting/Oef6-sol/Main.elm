-- Main module for excercise 6
-- Daan Seynaeve 2015
module Main where

import Signal
import Html exposing ( Html )
import Maybe
import Signal exposing (..)
import List exposing (length, take)

type alias CounterModel = Int
type CounterAction = Incr | Decr

type alias Model = List CounterModel
type Action = AddCounter | RemoveCounter | CounterAction Int

mailbox : Signal.Mailbox (Maybe Action)
mailbox = Signal.mailbox Nothing

main : Signal Html
main = map (view (Signal.forwardTo mailbox.address Just)) state

update : Action -> Model -> Model
update action model =
        case action of
                AddCounter -> model ++ [newcounter]
                RemoveCounter -> take ((length model)-1) model

newcounter : CounterModel
newcounter = 0

state : Signal Model
state = Signal.constant [1,2,3]

view : Address Action -> Model -> Html
view x m = Html.div [] (
                [
                Html.div [] [
                        Html.button [] [Html.text "Add"],
                        Html.button [] [Html.text "Remove"]
                        ]
                ]
                ++
                (List.map (\x -> Html.div [] [Html.text (toString x)]) m)
        )
