-- Write a program that shows three text fields, arranged vertically.
-- The first shows the current mouse position and True or False
-- depending on whether the left mouse button is currently down. The
-- second text box is shown below the first and shows True or False
-- depending on whether or not the space bar is down. Finally, the
-- third text field shows the current value of Keyboard.arrows. Play
-- with the resulting program so that you understand the behaviour of
-- all these input signals.

import Keyboard
import Mouse
import Html exposing ( Html )
import Signal

mouse_info : Signal String
mouse_info = Signal.map2 (pretty_mouse) Mouse.position Mouse.isDown

pretty_mouse a b = (toString a) ++ " - " ++ (toString b)

space_info = Signal.map (toString) Keyboard.space

arrow_info = Signal.map (toString) Keyboard.arrows

main : Signal Html
main = Signal.map3 view mouse_info space_info arrow_info

view : String -> String -> String -> Html
view x y z = Html.div [] [
        Html.div [] [ Html.text x ],
        Html.div [] [ Html.text y ],
        Html.div [] [ Html.text z ]
        ]
