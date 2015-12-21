module Main where

import Debug
import Html exposing ( Html )
import Signal exposing ( map )
import Html.Attributes as A exposing ( rel, href )
import Keyboard as K
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing ( andThen, Task, onError )
import Result exposing ( Result )
import Time exposing ( every, second )
import Date
--
import CustomTools exposing ( ($), watchSignal, isDefined, (?), toMaybe )
import MailItem
import ReminderItem
import Static
import List exposing ( (::) )
import ItemDecorator as ID exposing ( decorate )
import ItemFeed as Feed
import ReminderForm as Form
import Shortcuts
import ItemManager
import MailFetcher

-- Name: Daan Seynaeve
-- Student ID: r0296224

-- Total hours: +30

-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: Works as expected


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: Works as expected


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Attempted
-- Summary: Added the deadline property.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Attempted
-- Summary: Currently only works for a locally stored version
-- of the JSON-file due to a permission issue.


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary: Works as expected (except the permission issue)
-- Only adds mails that it has not seen before in the current session.
-- Incremental change of the json is allowed, as well as replacing
-- its contents entirely.

-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed / Attempted / Unattempted
-- Summary:


-- * Come up with your own extension!
-- Status: Completed / Attempted / Unattempted
-- Summary: Button to change look-and-feel of the application.

-- Start of program

-- # Model #

type alias Model = ItemManager.Model
type alias Action = ItemManager.Action

main : Signal Html.Html
main = Signal.map (view mailbox.address) ("MAIN STATE" $ state)
-- main = Signal.map (sandview) sandstate
state : Signal Model
state = Signal.foldp update init ("MASTER" $ master)

-- ### Sandbox ###
-- sandview : String -> Html
-- sandview s = Html.text s
--
-- sandstate = Signal.foldp sandupdate "test" sandmaster
--
-- sandmaster = Signal.map (toString << Date.fromTime) <| every second
--
-- sandupdate a m = a
--
-- sandbox : Signal.Mailbox (Maybe String)
-- sandbox = Signal.mailbox Nothing

-- ### Initizalization ###

init : Model
init = ItemManager.init initFeed Form.init

initFeed : Feed.Model
initFeed = Feed.init <|
    (List.map ((decorate ID.AMail) << MailItem.init) Static.emails) ++
    (List.map ((decorate ID.AReminder) << ReminderItem.init) Static.reminders)

-- ### Signals ###

master : Signal (Maybe Action)
master = Signal.mergeMany <|
        ["MAILBOX"          $ mailbox.signal]
    ++  ["NEW MAIL"         $ makeBatch MailFetcher.signal]
    ++  ["LOCAL MAILBOXES"  $ makeFormUpdate Form.signal]
    ++  ["SHORTCUT"         $ Shortcuts.signal]

mailbox : Signal.Mailbox (Maybe Action)
mailbox = Signal.mailbox Nothing

makeBatch = Signal.map (Just << ItemManager.FD << Feed.AddBatch)
makeFormUpdate = Signal.map (Just << ItemManager.FM)

-- ### Bind ports ###

port fetchMails : Signal (Task Http.Error ())
port fetchMails = MailFetcher.fetchMails

-- ### Update ###

update : Maybe Action -> Model -> Model
update action model =
    case action of
        Just a -> ItemManager.update a model
        Nothing -> model

-- ### View ###

view : Signal.Address (Maybe Action) -> Model -> Html
view address state =
    Html.main' [] [
        Html.header [] [
            css "style.css"
        ],
        Html.body [] [
            ItemManager.view (Signal.forwardTo address Just) state
        ]
    ]

-- ### Style ###

css : String -> Html
css path = Html.node "link" [ A.rel "stylesheet", A.href path ] []
