-- Create a button with as text an integer. Every time the button is pressed, the
-- integer should be incremented by one.

import Signal
import Html exposing ( Html )
import Html.Events as E

mailbox : Signal.Mailbox Int
mailbox = Signal.mailbox 0

state : Signal Int
state = Signal.foldp (+) 0 mailbox.signal

main : Signal Html
main = Signal.map (view mailbox.address) (Signal.map toString state)

view : Signal.Address Int -> String -> Html
view adr x = Html.div [] [
                Html.button [E.onClick adr 5] [Html.text "pophogen"],
                Html.text x
        ]
