module CustomTools where

import List
import Debug
import Html exposing (Html)
import Date exposing (Date, day, month, year, fromTime, toTime, fromString )
import Time exposing (Time)

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
($) = watchSignal
infixr 9 $

(?) : a -> Maybe a -> a
(?) default mb = Maybe.withDefault default mb
infixr 8 ?

header : String -> Html
header s = Html.h2 [] [Html.text s]

-- ### Working with dates ###

formatDate : Time -> String
formatDate time =
    let date = fromTime time
        month' = (monthToInt << month) date
        day' = day date
        leading0 x = if x < 10 then "0" else ""
    in  ((toString << year) date) ++ "-" ++
        (leading0 month') ++ (toString month') ++ "-" ++
        (leading0 day') ++ (toString day')

parseDate : String -> Time
parseDate dateString =
    toTime <| Result.withDefault (fromTime 0) (fromString dateString)

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
