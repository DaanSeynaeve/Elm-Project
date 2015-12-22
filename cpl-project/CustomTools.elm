module CustomTools where

import List
import Debug
import Html exposing (Html)
import Date exposing (Date, day, month, year)

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

monthToInt m = case m of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12

formatDate : Date -> String
formatDate date = ((toString << year) date)
        ++ "-" ++ ((toString << monthToInt << month) date)
        ++ "-" ++ ((toString << day) date)
