module CustomTools where

import List
import Debug
import Html exposing (Html)

zip : List x -> List y -> List (x,y)
zip xs ys = List.map2 (,) xs ys

isDefined : Maybe a -> Bool
isDefined maybe =
    case maybe of
        Just _ -> True
        _ -> False
toMaybe : a -> Bool -> Maybe a
toMaybe a bool = case bool of
    True -> Just a
    False -> Nothing

watchSignal : String -> Signal a -> Signal a
watchSignal caption = Signal.map (Debug.watch caption)

($) : String -> Signal a -> Signal a
($) caption s = Signal.map (Debug.watch caption) s
infixr 9 $

(?) : a -> Maybe a -> a
(?) default maybe = Maybe.withDefault default maybe
infixr 9 ?

nor x y = (||) (not x) (not y)

header : String -> Html
header s = Html.h2 [] [Html.text s]


-- type Hey = Hey | You

type alias Action = Maybe Verb
type Verb = Call | Met

you : Bool
you = True

thisis : String -> Bool
thisis s = case s of
    "crazy" -> True
    _ -> False

i : Action -> Bool -> Verb
i a _ = case a of
    Just v -> v
    Nothing -> Call

hey : (a -> b -> c) -> Verb -> Bool
hey _ v = case v of
    Call -> False
    Met -> True

hey2 : Verb -> Bool
hey2 = hey hey

test = ((hey hey) (i (Just Met) you)) && (thisis "crazy")
