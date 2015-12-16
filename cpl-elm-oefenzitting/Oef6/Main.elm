module Main where

import Signal
import Html exposing ( Html )
import CounterPair
import Maybe

mailbox : Signal.Mailbox (Maybe CounterPair.Action)
mailbox = Signal.mailbox Nothing

isDefined : Maybe a -> Bool
isDefined maybe =
  case maybe of
    Just _ -> True
    _ -> False

state : Signal CounterPair.Model
state =
  let update action model =
        case action of
          Just a -> CounterPair.update a model
          _ -> model
  in Signal.foldp update CounterPair.init mailbox.signal

main : Signal Html
main =
  let view = CounterPair.view (Signal.forwardTo mailbox.address Just)
  in Signal.map view state
