module Storage where

import List exposing ((::))
import Maybe exposing (andThen)
--
import CustomTools exposing ((?))
import ItemDecorator as ID exposing (decorate)
import MailItem
import ReminderItem
import ItemManager
import ReminderForm as Form
import ItemFeed as Feed

type alias Model = ItemManager.Model

type alias StorageItem = {
    item: (Maybe MailItem.Model, Maybe ReminderItem.Model),
    pinned: Bool,
    done: Bool
}

type alias StorageModel = List StorageItem

encodeItem : ID.Model -> StorageItem
encodeItem decorated = case decorated.item of
    ID.AMail m -> { decorated | item =(Just m,Nothing) }
    ID.AReminder r -> { decorated | item = (Nothing,Just r) }

decodeItem : StorageItem -> Maybe ID.Model
decodeItem storable = case storable.item of
    (Just m, _) -> Just {storable | item = ID.AMail m}
    (_, Just r) -> Just {storable | item = ID.AReminder r}
    _ -> Nothing

encode : Model -> StorageModel
encode model = List.map (encodeItem << snd) model.feed.items

decode : StorageModel -> Model
decode items =
    let maybeAppend x = identity ? Maybe.map (::) x
        feed = List.foldr maybeAppend [] (List.map decodeItem items)
    in  ItemManager.init (Feed.init feed) Form.init
