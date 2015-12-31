module ItemFeed where

import Signal
import Html exposing ( Html )
import Signal exposing ( map )
import List exposing ( (::), length, isEmpty, head )
import Html.Attributes as A
import Html.Events as E
import Date
import Maybe exposing ( andThen )
import Debug
--
import ItemDecorator as ID
import CustomTools as CT exposing ( (?) )

-- ### Model ###

type alias ItemList =
    List (Int, ID.Model)

type SortKey = Default | OldOnTop

type alias Model = {
    items : ItemList,
    focus : Int,
    skey  : SortKey,
    showTodo  : Bool
}

init : List ID.Model -> Model
init items = update (AddBatch items)
    {   items = []
    ,   focus = 0
    ,   skey = Default
    ,   showTodo = True
    }

-- ### Helper functions ###

-- return the itemlist of the model correctly sorted
sortedFeed : Model -> ItemList
sortedFeed model =
    let dateSorted = List.sortBy (ID.itemTime << snd) model.items
    in  case model.skey of
        Default -> (uncurry (++)) <| List.partition (.pinned << snd) <|
            List.reverse dateSorted
        OldOnTop -> dateSorted

-- split a given item list in 2 sublist: todo and done
splitFeed : ItemList -> (ItemList,ItemList)
splitFeed items = List.partition (not << .done << snd) items

-- returns the visible part of the item list correctly sorted
visible : Model -> ItemList
visible model =
    let (l1,l2) = splitFeed (sortedFeed model)
    in  if model.showTodo then l1++l2 else l1

-- maybe return the identifier for the item
-- at the given index in the given list
itemId : ItemList -> Int -> Maybe Int
itemId items i = Maybe.map fst <| head <| List.drop i items

-- returns the identifier for the focused item in the
-- visible list if it is visibile otherwise (-1)
getFid : Model -> Int
getFid model = (-1) ? itemId (visible model) model.focus

-- ### Update ###

type Action
    = ItemAction (Int, ID.Action)
    | FocusAction ID.Action
    | MapItemAction ID.Action
    | ChangeFocus FocusDirection
    | SortAction SortKey
    | AddItem ID.Model
    | AddBatch (List ID.Model)
    | ToggleDoneVis

type FocusDirection = Prev | Next

update : Action -> Model -> Model
update action model = updateFocus <| case action of
    ItemAction a    -> updateItem a model
    FocusAction a   -> updateItem ((getFid model),a) model
    MapItemAction a ->
        let mapSnd f = List.map (\(x,y) -> (x, f y))
        in  { model | items = mapSnd (ID.update a) model.items }

    AddItem item    -> addItem item model
    AddBatch items  -> List.foldr addItem model items

    ChangeFocus fd  -> changeFocus fd model
    ToggleDoneVis   -> { model | showTodo = not model.showTodo }
    SortAction key  -> { model | skey = key }

containsItem : ID.Model -> Model -> Bool
containsItem item model =
    List.any (ID.itemEqual item << snd) model.items

addItem : ID.Model -> Model -> Model
addItem item model =
    if not (containsItem item model)
    then { model | items = (length model.items,item)::model.items }
    else model

updateItem : (Int,ID.Action) -> Model -> Model
updateItem (id,a) model =
    let items' =
        List.map
            (\(i,item) ->
                if (id == i) then (i, ID.update a item) else (i,item))
            model.items
    in { model | items = items' }

updateFocus : Model -> Model
updateFocus model =
    { model | focus = min model.focus ((length (visible model))-1) }

changeFocus : FocusDirection -> Model -> Model
changeFocus fdir model =
    let len = length (visible model)
        focus' = if len == 0 then model.focus
            else case fdir of
                Next -> (model.focus + 1) % len
                Prev -> (model.focus - 1) % len
    in { model | focus = focus' }

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address model =
    let (l1,l2) = splitFeed (sortedFeed model)
        fid = getFid model
        subview = viewItems address fid
    in Html.div [] <|
        (if not (isEmpty l1)
            then subview "To Do" l1 else []) ++
        (if not (isEmpty l2) && model.showTodo
            then subview "Done" l2 else [])

viewItems : Signal.Address Action -> Int -> String -> ItemList -> List Html
viewItems address fid title items =
    let tag id a = ItemAction (id,a)
        newAddress id = Signal.forwardTo address (tag id)
        subview id = ID.view (newAddress id)
    in [CT.header title] ++
        List.map (\(id,item) -> wrapItem (id==fid) (subview id item)) items

wrapItem : Bool -> Html -> Html
wrapItem focus item = Html.div
    [A.class (if focus then "focus itemwrap" else "itemwrap")] [item]
