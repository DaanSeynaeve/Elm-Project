-- Modify the main program below, so that it shows the text "Time:
-- 12345678" instead of "Hello World!". The value 12345678 should be
-- the current value of the time signal.

import Signal exposing ( map )
import Time
import Html exposing ( Html )

time : Signal String
time = Signal.map toString <| Time.every Time.second

main : Signal Html
main = (map view time)

view : String -> Html
view x = Html.text ("Hello World" ++ x)
