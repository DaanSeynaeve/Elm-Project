-- Step 1: Change the below code so that it counts the number of user clicks.
-- Step 2: Change the code so that it counts the number of user clicks and space
-- bar presses. (make sure you don't count space bar presses twice!)

-- Look at Mouse.clicks and Keyboard.space

import Signal
import Mouse
import Keyboard
import Html exposing ( Html )

state : Signal Int
state = Signal.map2 (+) (Signal.foldp (\_ c -> c + 1) 0 Mouse.clicks)
        (Signal.foldp (\x c -> c + (if x then 1 else 0)) 0 Keyboard.space)

view : Int -> Html
view int = Html.text <| "Current count: " ++ (toString int)

main : Signal Html
main = Signal.map view state
