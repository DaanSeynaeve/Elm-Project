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
signal = Signal.foldp dropOld [] <|
    Signal.dropRepeats newMails.signal

dropOld : List x -> List x -> List x
dropOld new old = List.filter (\x -> not(List.member x old)) new

-- ###

makeMail : Static.Email -> ID.Model
makeMail email =
    ID.decorate ID.AMail (MailItem.init email)

newMails : Signal.Mailbox (List ID.Model)
newMails = Signal.mailbox []

sourceUrl : String
sourceUrl = "http://localhost:8000/json/newmails.json"
-- sourceUrl = "http://people.cs.kuleuven.be/~bob.reynders/2015-2016/emails.json"

fetchMails : Signal (Task Http.Error ())
fetchMails = Signal.map lookupMails (every (1*second))

lookupMails _ =
    Http.get reader sourceUrl
    `andThen` (\task -> Signal.send newMails.address (List.map makeMail task))

--analyze x = case x of
--    Http.Timeout -> "timout"
--    Http.NetworkError -> "net"
--    Http.UnexpectedPayload s -> "JSON" ++ s
--    Http.BadResponse _ _ -> "response"

reader : Json.Decoder (List Static.Email)
reader = ((:=) "emails") <| Json.list <| Json.object5 Static.Email
        ("from" := Json.string)
        ("to" := Json.string)
        ("title" := Json.string)
        ("body" := Json.string)
        ("date" := Json.string)
