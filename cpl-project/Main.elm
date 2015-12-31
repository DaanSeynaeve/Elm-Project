module Main where

-- Name: Daan Seynaeve
-- Student ID: r0296224
-- Total hours: +45

-- DISCLAIMERS:
-- (1) Because of the local storage functionality, the build procedure differs
-- from what was mentioned in the assignment. Build using the following:
--  > sh Build.sh
--
-- (2) Marking an item as done keeps the focus on the same position similar
-- to the behaviour under pinning & sorting.
--
-- (3) Toggling the visibility of the 'done' items changes the length of the
-- the feed and therefore the focus has to change sometimes. In this case,
-- the focus will change to the lowest visible item.


-- * Add a hotkey to toggle the visibility of 'done' items.
-- Status: Completed
-- Summary: Works as expected. Use ALT+V


-- * Hide the 'add reminder' functionality and add a hotkey to toggle its
-- * visibility.
-- Status: Completed
-- Summary: Works as expected. Use ALT+F


-- * Put the current date as the default in the date picker when adding
-- * reminders.
-- Status: Completed
-- Summary: Works as expected.


-- * Add a deadline property to reminders and mark all reminders that are past
-- * their deadline.
-- Status: Completed
-- Summary: Added the deadline property. A deadline is optional and
--      reminders without are displayed with "deadline: None"
--      Reminders that are overdue have their deadline highlighted.


-- * Add a 'snooze' feature to items, to 'snooze' an item you must provide a
-- * date on which the item has to 'un-snooze'. 'snoozed' items are not visible.
-- Status: Unattempted
-- Summary: /


-- * On startup, read e-mails from a Json document at this url:
-- * http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json
-- Status: Completed
-- Summary: On my local machine, I was unable to retrieve the JSON file directly
--      due to a permission issue: there is no Access-Control-Allow-Origin header
--      present on the recource, which causes my browser to block the request
--      because of the Same-Origin policy. I therefore tested everything with
--      a locally hosted version of the json.
--      As suggested on the toledo discussion board, I later changed this
--      to https://api.myjson.com/bins/19lg3.
--      The URL can be configured in ./MailFetcher.elm


-- * Periodically check for e-mails from Json (same url).
-- Status: Completed
-- Summary: Works as expected
--      Only adds mails that it has not seen before in the current
--      session. Incremental change of the json is allowed, as well as
--      replacing its contents entirely. A check happens every 30 seconds.


-- * Add persistence to your application by using Html local storage so that
-- * newly added reminders are still there after a reload.
-- Status: Completed
-- Summary: Works but requires a slighly different build procedure (see disclaimer 1)
--      The following is persisted:
--      - emails, reminders & added properties (e.g. done, pinned)
--      Storage.elm exists to map the application model to the storage model
--      Union types are represented with a tuple of Maybe's


-- * Come up with your own extension!
-- Status: Completed
-- Summary: Button to change look-and-feel of the application.
--      Works as expected.
-- Motivaton: I strongly agree with the thesis that any web application
--      should have content and style cleanly separated. To this end,
--      I wanted to see how easy it is to use .css stylesheets
--      instead of typing up style attributes in elm syntax.


-- Start of program

import List
import Html exposing ( Html )
import Signal
import Html.Attributes as A
import Keyboard as K
import Http
import Task exposing ( Task )
import Time exposing ( Time, every, second )
--
import CustomTools exposing ( (?) )
import Static
import MailItem
import ReminderItem
import ItemDecorator as ID exposing ( decorate )
import ItemFeed as Feed
import ReminderForm as Form
import ItemManager
import Shortcuts
import MailFetcher
import Storage

-- ### Model ###

type alias Model = ItemManager.Model
type alias Action = ItemManager.Action

main : Signal Html.Html
main = Signal.map (view mailbox.address) state

state : Signal Model
state = Signal.foldp update init master

-- ### Initialization ###

init : Model
init = initNew ? (Maybe.map Storage.decode getStorage)

initNew =
    let mailItems = List.map ((decorate ID.AMail) << MailItem.init)
            Static.emails
        reminderItems = List.map ((decorate ID.AReminder) << ReminderItem.init)
            Static.reminders
        feed = Feed.init <| mailItems ++ reminderItems
    in ItemManager.init feed Form.init

-- ### Signals ###

master : Signal (Maybe Action)
master = Signal.mergeMany
    [   mailbox.signal
    ,   newMails
    ,   timing
    ,   Shortcuts.signal
    ]

mailbox : Signal.Mailbox (Maybe Action)
mailbox = Signal.mailbox Nothing

newMails : Signal (Maybe Action)
newMails = Signal.map (Just << ItemManager.FD << Feed.AddBatch) <|
    MailFetcher.signal

timing : Signal (Maybe Action)
timing = Signal.map (Just << ItemManager.ChangeTime) <|
    every second

-- ### Bind ports ###

-- getStorage = Nothing -- uncomment for the version without storage

port getStorage : Maybe Storage.StorageModel -- uncomment for storage version

port setStorage : Signal Storage.StorageModel -- uncomment for storage version
port setStorage = Signal.map Storage.encode state -- uncomment for storage version

port fetchMails : Signal (Task Http.Error ())
port fetchMails = MailFetcher.fetchMails

-- ### Update ###

update : Maybe Action -> Model -> Model
update maybeAction model =
    case maybeAction of
        Just action -> ItemManager.update action model
        Nothing -> model

-- ### View ###

view : Signal.Address (Maybe Action) -> Model -> Html
view address model =
    Html.div [] [
        css model.stylesheet,
        ItemManager.view (Signal.forwardTo address Just) model
    ]

-- ### Style ###

css : String -> Html
css path =
    Html.node "link" [ A.rel "stylesheet", A.href path ] []
