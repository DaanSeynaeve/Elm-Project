module MailFetcher where

import Http
import Json.Decode as Json exposing ((:=))
import Task exposing ( andThen, Task, onError )
import List
import Time exposing ( every, second, minute )
---
import ItemDecorator as ID
import MailItem
import Static

-- ### Outgoing signal ###

signal : Signal (List ID.Model)
signal = newMails.signal

-- ###

makeMail : Static.Email -> ID.Model
makeMail email =
    ID.decorate ID.AMail (MailItem.init email)

newMails : Signal.Mailbox (List ID.Model)
newMails = Signal.mailbox []

fetchMails : Signal (Task Http.Error ())
fetchMails = Signal.map lookupMails (every (30*second))

sourceUrl : String
sourceUrl = "https://api.myjson.com/bins/19lg3"
-- sourceUrl = "http://localhost:8000/json/emails.json" -- debug
-- sourceUrl = "http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"

lookupMails _ =
    Http.get reader sourceUrl
    `andThen` (\task -> Signal.send newMails.address (List.map makeMail task))

reader : Json.Decoder (List Static.Email)
reader = ((:=) "emails") <| Json.list <|
    Json.object5 Static.Email
        ("from" := Json.string)
        ("to" := Json.string)
        ("title" := Json.string)
        ("body" := Json.string)
        ("date" := Json.string)
