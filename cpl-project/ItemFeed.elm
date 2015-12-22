module ItemFeed where

import Signal
import Html exposing ( Html )
import Signal exposing ( map )
import List exposing ( (::), length, isEmpty, head )
import Html.Attributes as A
import Html.Events as E
import Date
import Maybe exposing ( andThen )
--
import ItemDecorator
import CustomTools as CT exposing ( (?) )

-- ### Model ###

type alias ItemList = List (Int, ItemDecorator.Model)
type SortKey = Default | OldOnTop

type alias Model = {
    items : ItemList,
    focus : Int,
    skey  : SortKey,
    dvis  : Bool
}

init : List ItemDecorator.Model -> Model
init items = {
        items = List.map2 (,) [0..List.length(items)-1] items,
        focus = 0,
        skey = Default,
        dvis = True
    }

-- return the itemlist of the model correctly sorted
sortedFeed : Model -> ItemList
sortedFeed model = case model.skey of
    Default -> (uncurry (++)) <| List.partition (.pinned << snd) model.items
    OldOnTop -> List.sortBy (ItemDecorator.dateString << snd) model.items

-- split a given item list in 2 sublist: todo and done
splitFeed : ItemList -> (ItemList,ItemList)
splitFeed items = List.partition (not << .done << snd) items

-- maybe return the identifier for the item at the given index
itemId : ItemList -> Int -> Maybe Int
itemId items i = Maybe.map fst <| head <| List.drop i items

-- returns the fid for given model
getFid : Model -> Int
getFid model =
    let (l1,l2) = splitFeed (sortedFeed model)
    in  (-1 ? (head l1 `andThen` (Just << fst))) ?
        (itemId (if model.dvis then l1++l2 else l1) model.focus)

-- ### Update ###

type Action = ItemAction (Int, ItemDecorator.Action)
            | FocusAction ItemDecorator.Action
            | ChangeFocus FocusDirection
            | SortAction SortKey
            | AddItem ItemDecorator.Model
            | AddBatch (List ItemDecorator.Model)
            | ToggleDoneVis

type FocusDirection = Prev | Next

update : Action -> Model -> Model
update action model = case action of
        ItemAction ia   -> updateItem ia model
        FocusAction fa  -> updateItem ((getFid model),fa) model
        ChangeFocus fd  -> updateFocus fd model
        SortAction skey -> { model | skey = skey }
        AddItem item    -> addItem item model
        ToggleDoneVis   -> { model | dvis = not model.dvis }
        AddBatch items  -> List.foldl addItem model items

addItem : ItemDecorator.Model -> Model -> Model
addItem item model =
    { model | items = (length model.items,item)::model.items }

updateItem : (Int,ItemDecorator.Action) -> Model -> Model
updateItem ia model =
    let items' =
        List.map (\(i,x) ->
            if (i == (fst ia))
            then (i,ItemDecorator.update (snd ia) x)
            else (i,x)) model.items
    in { model | items = items' }

updateFocus : FocusDirection -> Model -> Model
updateFocus fd model =
    let focus' =
        let len = length (
            if model.dvis
            then model.items
            else fst (splitFeed model.items) )
        in if len == 0 then model.focus else case fd of
            Next -> (model.focus + 1) % len
            Prev -> (model.focus - 1) % len
    in { model | focus = focus' }

-- ### View ###

view : Signal.Address Action -> Model -> Html
view address state =
    let (l1,l2) = splitFeed (sortedFeed state)
        fid = getFid state
        subview = viewItems address fid
    in Html.div [] <|
        (subview "To Do" l1) ++
        if (not (isEmpty l2)) && state.dvis
        then subview "Done" l2
        else []

viewItems : Signal.Address Action -> Int -> String -> ItemList -> List Html
viewItems address fid title items =
    let tag i a = ItemAction (i,a)
    in [CT.header title] ++
        List.map
        (\(i,x) -> wrapItem (i==fid)
            (ItemDecorator.view (Signal.forwardTo address (tag i)) x))
        items

wrapItem : Bool -> Html -> Html
wrapItem focus item = Html.div
    [A.class (if focus then "focus itemwrap" else "itemwrap")] [item]
