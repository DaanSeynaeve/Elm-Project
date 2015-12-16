module Shortcuts where

import Keyboard as K
import Signal
--
import ItemFeed as Feed
import ItemDecorator as ID
import ItemManager as M
import MailItem
import CustomTools exposing ( toMaybe )

type alias Action = M.Action
-- type alias FD = M.FD

signal : Signal (Maybe Action)
signal = Signal.merge
    (Signal.dropRepeats alt_shortcuts)
    (Signal.dropRepeats alt_s)

alt = K.shift -- ALT modifier

alt_shortcuts : Signal (Maybe Action)
alt_shortcuts = Signal.mergeMany <|
    List.map (\(k,act) ->
        let f x y = (toMaybe act) (x && y)
        in Signal.dropRepeats <| Signal.map2 f alt k
    )   [   -- default hotkeys
            (K.isDown 74, M.FD <| Feed.ChangeFocus Feed.Next)       -- J
        ,   (K.isDown 75, M.FD <| Feed.ChangeFocus Feed.Prev)       -- K
        ,   (K.isDown 79, M.FD <| Feed.FocusAction <|               -- O
                            ID.MailAction MailItem.ToggleCollapse)
        ,   (K.isDown 80, M.FD <| Feed.FocusAction ID.TogglePinned) -- P
        ,   (K.isDown 88, M.FD <| Feed.FocusAction ID.ToggleDone)   -- X

            -- custom hotkeys
        ,   (K.isDown 86, M.FD <| Feed.ToggleDoneVis )              -- V
        ,   (K.isDown 70, M.ToggleVisForm)                          -- F
        ]

alt_s : Signal (Maybe Action)
alt_s = Signal.map2 (\ak s -> Just <| M.FD <| Feed.SortAction <|
            case ak && s of
                False -> Feed.Default
                True -> Feed.OldOnTop
            )
        alt (K.isDown 83)
