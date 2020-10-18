module SelectRing exposing
    ( SelectRing
    , empty, singleton, fromList, fromArray
    , push, append, prepend
    , removeAt, removeFirst, removeLast, removeFocused, removeSelected
    , focusOn, focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
    , focusOnNextMatching, focusOnPreviousMatching, focusOnFirstMatching, focusOnLastMatching
    , selectAt, selectFirst, selectLast, selectFocused
    , selectFirstMatching
    , deselectAt, deselectFirst, deselectLast, deselectFocused
    , clearSelected, deselectMatching
    , toggleAt, toggleFirst, toggleLast, toggleFocused
    , isEmpty
    , isNoneSelected, isAnySelected, isSelectedAt, isSelectedMatching
    , isFocusedAt, isFocusedMatching
    , size
    , get, getFirst, getLast, getFocused, getFocusedIndex, getSelected, getSelectedIndex
    , set, setFocused, setSelected
    , toList, toArray
    , map
    , mapFocused, mapEachIntoList, mapEachIntoArray
    )

{-| A ring containing at most a single selected element

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and selecting at most a single element from it.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


# Definition

@docs SelectRing


# Construct a SelectRing

@docs empty, singleton, fromList, fromArray


# Insert elements

@docs push, append, prepend


# Remove elements

@docs removeAt, removeFirst, removeLast, removeFocused, removeSelected


# Focus on an element

@docs focusOn, focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
@docs focusOnNextMatching, focusOnPreviousMatching, focusOnFirstMatching, focusOnLastMatching


# Select an element

@docs selectAt, selectFirst, selectLast, selectFocused
@docs selectFirstMatching


# Deselect an element

@docs deselectAt, deselectFirst, deselectLast, deselectFocused
@docs clearSelected, deselectMatching


# Toggle an element selection

@docs toggleAt, toggleFirst, toggleLast, toggleFocused


# Predicates

@docs isEmpty
@docs isNoneSelected, isAnySelected, isSelectedAt, isSelectedMatching
@docs isFocusedAt, isFocusedMatching


# Accessors

@docs size
@docs get, getFirst, getLast, getFocused, getFocusedIndex, getSelected, getSelectedIndex
@docs set, setFocused, setSelected


# Transform

@docs toList, toArray
@docs map
@docs mapFocused, mapEachIntoList, mapEachIntoArray

-}

import Array exposing (Array)
import Array.Extra as Array
import Maybe
import Maybe.Extra as Maybe



-- Definition


type SelectRing a
    = SelectRing
        { elements : Array a
        , focused : Int
        , selected : Maybe Int
        }



-- Construct a SelectRing


{-| Construct an empty SelectRing.
-}
empty : SelectRing a
empty =
    SelectRing
        { elements = Array.empty
        , focused = 0
        , selected = Nothing
        }


{-| Construct a SelectRing from a List (which might be empty).
-}
fromList : List a -> SelectRing a
fromList list =
    SelectRing
        { elements = Array.fromList list
        , focused = 0
        , selected = Nothing
        }


{-| Construct a SelectRing from an Array (which might be empty).
-}
fromArray : Array a -> SelectRing a
fromArray array =
    SelectRing
        { elements = array
        , focused = 0
        , selected = Nothing
        }


{-| Construct a SelectRing containing a single focused element.
-}
singleton : a -> SelectRing a
singleton element =
    push element empty



-- Insert elements


{-| Insert an element at the end of the SelectRing.
-}
push : a -> SelectRing a -> SelectRing a
push element (SelectRing ring) =
    SelectRing
        { ring
            | elements = Array.push element ring.elements
        }


{-| Insert several elements at the end of the SelectRing.
-}
append : List a -> SelectRing a -> SelectRing a
append elements (SelectRing ring) =
    SelectRing
        { ring
            | elements = Array.append ring.elements (Array.fromList elements)
        }


{-| Insert several elements at the beginning of the SelectRing.
Current focus and selected elements are maintained.
-}
prepend : List a -> SelectRing a -> SelectRing a
prepend elements (SelectRing ring) =
    let
        shift =
            List.length elements
    in
    SelectRing
        { ring
            | elements = Array.append (Array.fromList elements) ring.elements
            , focused = ring.focused + shift
            , selected = Maybe.map ((+) shift) ring.selected
        }



-- Remove elements


{-| Remove the element at the provided index (modulo the ring size).

If the focused element is removed, the focus is put on the next element, unless it is the last
element of the ring. In that case, the focus is put on previous element instead. This element also
becomes the last one in the ring.

If the selected element is removed, then Nothing is selected anymore.

-}
removeAt : Int -> SelectRing a -> SelectRing a
removeAt index ((SelectRing ring) as selectRing) =
    let
        ringSize =
            size selectRing

        removedIndex =
            modBy ringSize index

        focusedIndex =
            if removedIndex == ringSize - 1 then
                modBy ringSize (ring.focused - 1)

            else
                ring.focused

        selectedIndex =
            ring.selected
                |> Maybe.andThen adaptSelectedIndex

        adaptSelectedIndex selected =
            if selected == removedIndex then
                Nothing

            else if selected > removedIndex then
                Just (selected - 1)

            else
                Just selected
    in
    SelectRing
        { ring
            | elements = Array.removeAt removedIndex ring.elements
            , focused = focusedIndex
            , selected = selectedIndex
        }


{-| Remove the first element of the ring.
If the focused element is removed, the focus is put on the next element.
If the selected element is removed, then Nothing is selected anymore.
-}
removeFirst : SelectRing a -> SelectRing a
removeFirst selectRing =
    removeAt 0 selectRing


{-| Remove the last element of the ring.
If the focused element is removed, the focus is put on the previous element.
If the selected element is removed, then Nothing is selected anymore.
-}
removeLast : SelectRing a -> SelectRing a
removeLast selectRing =
    removeAt (size selectRing - 1) selectRing


{-| Remove the focused element from the ring.

The focus is put on the next element, unless it is the last element of the ring. In that case,
the focus is put on previous element instead. This element also becomes the last one in the ring.

If the selected element is removed, then Nothing is selected anymore.

-}
removeFocused : SelectRing a -> SelectRing a
removeFocused ((SelectRing ring) as selectRing) =
    removeAt ring.focused selectRing


{-| Remove the selected element from the ring. Nothing is selected anymore.

If the focused element is removed, the focus is put on the next element, unless it is the last
element of the ring. In that case, the focus is put on previous element instead. This element also
becomes the last one in the ring.

-}
removeSelected : SelectRing a -> SelectRing a
removeSelected ((SelectRing ring) as selectRing) =
    ring.selected
        |> Maybe.map (\index -> removeAt index selectRing)
        |> Maybe.withDefault selectRing



-- Focus


{-| Focus on the element at the provided index (modulo the ring size).
-}
focusOn : Int -> SelectRing a -> SelectRing a
focusOn index ((SelectRing ring) as selectRing) =
    SelectRing
        { ring
            | focused = modBy (size selectRing) index
        }


{-| Focus on the next element in the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : SelectRing a -> SelectRing a
focusOnNext ((SelectRing ring) as selectRing) =
    SelectRing
        { ring
            | focused = modBy (size selectRing) (ring.focused + 1)
        }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : SelectRing a -> SelectRing a
focusOnPrevious ((SelectRing ring) as selectRing) =
    SelectRing
        { ring
            | focused = modBy (size selectRing) (ring.focused - 1)
        }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : SelectRing a -> SelectRing a
focusOnFirst selectRing =
    focusOn 0 selectRing


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : SelectRing a -> SelectRing a
focusOnLast selectRing =
    focusOn (size selectRing - 1) selectRing


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnNextMatching predicate ((SelectRing ring) as selectRing) =
    let
        indexedRing =
            Array.toIndexedList ring.elements

        nextMatch =
            indexedRing
                |> List.filter
                    (\( index, element ) ->
                        predicate element && index > ring.focused
                    )
                |> List.head

        previousMatch =
            indexedRing
                |> List.filter
                    (\( index, element ) ->
                        predicate element && index <= ring.focused
                    )
                |> List.head

        matchingIndex =
            nextMatch
                |> Maybe.orElse previousMatch
                |> Maybe.map Tuple.first
    in
    matchingIndex
        |> Maybe.map (\index -> focusOn index selectRing)
        |> Maybe.withDefault selectRing


{-| Focus on the previous element in the ring that matches the provided predicate.
-}
focusOnPreviousMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnPreviousMatching predicate ((SelectRing ring) as selectRing) =
    let
        indexedRing =
            Array.toIndexedList ring.elements

        previousMatch =
            indexedRing
                |> List.filter
                    (\( index, element ) ->
                        predicate element && index <= ring.focused
                    )
                |> List.reverse
                |> List.head

        nextMatch =
            indexedRing
                |> List.filter
                    (\( index, element ) ->
                        predicate element && index > ring.focused
                    )
                |> List.reverse
                |> List.head

        matchingIndex =
            previousMatch
                |> Maybe.orElse nextMatch
                |> Maybe.map Tuple.first
    in
    matchingIndex
        |> Maybe.map (\index -> focusOn index selectRing)
        |> Maybe.withDefault selectRing


{-| Focus on the first element in the ring that matches the provided predicate.
-}
focusOnFirstMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnFirstMatching predicate selectRing =
    selectRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index selectRing)
        |> Maybe.withDefault selectRing


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnLastMatching predicate selectRing =
    selectRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index selectRing)
        |> Maybe.withDefault selectRing



-- Select an element


{-| Select the element at the provided index (modulo the ring size).
This replaces the currently selected element.
-}
selectAt : Int -> SelectRing a -> SelectRing a
selectAt index ((SelectRing ring) as selectRing) =
    let
        selectedIndex =
            modBy (size selectRing) index
    in
    SelectRing
        { ring
            | selected = Just selectedIndex
        }


{-| Select the first element of the ring. This replaces the currently selected element.
-}
selectFirst : SelectRing a -> SelectRing a
selectFirst selectRing =
    selectAt 0 selectRing


{-| Select the last element of the ring. This replaces the currently selected element.
-}
selectLast : SelectRing a -> SelectRing a
selectLast selectRing =
    selectAt (size selectRing - 1) selectRing


{-| Select the currently focused element. This replaces the currently selected element.
-}
selectFocused : SelectRing a -> SelectRing a
selectFocused selectRing =
    selectAt (getFocusedIndex selectRing) selectRing


{-| Select the first element in the ring that matches the provided predicate.
This replaces the currently selected element.
-}
selectFirstMatching : (a -> Bool) -> SelectRing a -> SelectRing a
selectFirstMatching predicate selectRing =
    let
        matchingIndex =
            selectRing
                |> toArray
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
                |> List.head
    in
    matchingIndex
        |> Maybe.map (\index -> selectAt index selectRing)
        |> Maybe.withDefault selectRing



-- Deselect an element


{-| Deselect the currently selected element.
-}
clearSelected : SelectRing a -> SelectRing a
clearSelected (SelectRing ring) =
    SelectRing
        { ring
            | selected = Nothing
        }


{-| Deselect element at the provided index (modulo the ring size).
If the element is already deselected, the same SelectRing is returned unchanged.
-}
deselectAt : Int -> SelectRing a -> SelectRing a
deselectAt index selectRing =
    if isSelectedAt index selectRing then
        clearSelected selectRing

    else
        selectRing


{-| Deselect the first element of the ring (if selected).
-}
deselectFirst : SelectRing a -> SelectRing a
deselectFirst selectRing =
    deselectAt 0 selectRing


{-| Deselect the last element of the ring (if selected).
-}
deselectLast : SelectRing a -> SelectRing a
deselectLast selectRing =
    deselectAt (size selectRing - 1) selectRing


{-| Deselect the currently focused element.
-}
deselectFocused : SelectRing a -> SelectRing a
deselectFocused selectRing =
    deselectAt (getFocusedIndex selectRing) selectRing


{-| Deselect currently selected element if it matches the provided predicate.
-}
deselectMatching : (a -> Bool) -> SelectRing a -> SelectRing a
deselectMatching predicate selectRing =
    getSelected selectRing
        |> Maybe.filter (\selected -> predicate selected)
        |> Maybe.map (\_ -> clearSelected selectRing)
        |> Maybe.withDefault selectRing



-- Toggle an element selection


{-| Toggle selection of the element at the provided index (modulo the ring size).
-}
toggleAt : Int -> SelectRing a -> SelectRing a
toggleAt index selectRing =
    if isSelectedAt index selectRing then
        deselectAt index selectRing

    else
        selectAt index selectRing


{-| Toggle selection of the first element in the ring.
-}
toggleFirst : SelectRing a -> SelectRing a
toggleFirst selectRing =
    toggleAt 0 selectRing


{-| Toggle selection of the last element in the ring.
-}
toggleLast : SelectRing a -> SelectRing a
toggleLast selectRing =
    toggleAt (size selectRing - 1) selectRing


{-| Toggle selection of the currently focused element.
-}
toggleFocused : SelectRing a -> SelectRing a
toggleFocused selectRing =
    toggleAt (getFocusedIndex selectRing) selectRing



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : SelectRing a -> Bool
isEmpty (SelectRing ring) =
    Array.isEmpty ring.elements


{-| Indicate whether or not the ring has no selected element.
-}
isNoneSelected : SelectRing a -> Bool
isNoneSelected (SelectRing ring) =
    Maybe.isNothing ring.selected


{-| Indicate whether or not the ring has a selected element.
-}
isAnySelected : SelectRing a -> Bool
isAnySelected (SelectRing ring) =
    Maybe.isJust ring.selected


{-| Indicate whether or not the element at the provided index (modulo the ring size) is selected.
-}
isSelectedAt : Int -> SelectRing a -> Bool
isSelectedAt index ((SelectRing ring) as selectRing) =
    let
        selectedIndex =
            modBy (size selectRing) index
    in
    ring.selected == Just selectedIndex


{-| Indicate whether or not the selected element matches the provided predicate.
-}
isSelectedMatching : (a -> Bool) -> SelectRing a -> Bool
isSelectedMatching predicate selectedRing =
    getSelected selectedRing
        |> Maybe.map predicate
        |> Maybe.withDefault False


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocusedAt : Int -> SelectRing a -> Bool
isFocusedAt index selectedRing =
    getFocusedIndex selectedRing == modBy (size selectedRing) index


{-| Indicate whether or not the focused element matches the provided predicate.
-}
isFocusedMatching : (a -> Bool) -> SelectRing a -> Bool
isFocusedMatching predicate selectedRing =
    getFocused selectedRing
        |> Maybe.map predicate
        |> Maybe.withDefault False



-- Accessors


{-| Return the size of the ring.
-}
size : SelectRing a -> Int
size (SelectRing ring) =
    Array.length ring.elements


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> SelectRing a -> Maybe a
get index ((SelectRing ring) as selectRing) =
    let
        elementIndex =
            modBy (size selectRing) index
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : SelectRing a -> Maybe a
getFirst selectRing =
    get 0 selectRing


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : SelectRing a -> Maybe a
getLast selectRing =
    get (size selectRing - 1) selectRing


{-| Return the index of the focused element in the ring.
-}
getFocusedIndex : SelectRing a -> Int
getFocusedIndex (SelectRing ring) =
    ring.focused


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : SelectRing a -> Maybe a
getFocused selectRing =
    get (getFocusedIndex selectRing) selectRing


{-| Return Just the selected element. When none are selected, Nothing is returned instead.
-}
getSelectedIndex : SelectRing a -> Maybe Int
getSelectedIndex (SelectRing ring) =
    ring.selected


{-| Return Just the selected element. When none are selected, Nothing is returned instead.
-}
getSelected : SelectRing a -> Maybe a
getSelected ((SelectRing ring) as selectRing) =
    ring.selected
        |> Maybe.andThen (\index -> get index selectRing)


{-| Set the element of the ring at the provided index (modulo the ring size) and return an updated
SelectRing without changing current focus and selection states.
-}
set : Int -> a -> SelectRing a -> SelectRing a
set index element ((SelectRing ring) as selectRing) =
    let
        elementIndex =
            modBy (size selectRing) index
    in
    SelectRing
        { ring
            | elements =
                Array.set elementIndex element ring.elements
        }


{-| Set the focused element of the ring and return an updated SelectRing.
-}
setFocused : a -> SelectRing a -> SelectRing a
setFocused element selectRing =
    selectRing
        |> set (getFocusedIndex selectRing) element


{-| Set the selected element of the ring (if any) and return an updated SelectRing.
If none is selected return the unchanged SelectRing instead.
-}
setSelected : a -> SelectRing a -> SelectRing a
setSelected element ((SelectRing ring) as selectRing) =
    ring.selected
        |> Maybe.map (\index -> set index element selectRing)
        |> Maybe.withDefault selectRing



-- Transform


{-| Create a list of elements from the SelectRing.
-}
toList : SelectRing a -> List a
toList (SelectRing ring) =
    Array.toList ring.elements


{-| Create an array of elements from the SelectRing.
-}
toArray : SelectRing a -> Array a
toArray (SelectRing ring) =
    ring.elements


map : (a -> b) -> SelectRing a -> SelectRing b
map mutator (SelectRing ring) =
    SelectRing
        { elements = Array.map mutator ring.elements
        , focused = ring.focused
        , selected = ring.selected
        }


mapFocused : (a -> b) -> SelectRing a -> Maybe b
mapFocused mutator ring =
    Maybe.map mutator (getFocused ring)


mapEachIntoList : (a -> b) -> (a -> b) -> (a -> b) -> SelectRing a -> List b
mapEachIntoList basicMutator focusedMutator selectedMutator ring =
    Array.toList (mapEachIntoArray basicMutator focusedMutator selectedMutator ring)


mapEachIntoArray : (a -> b) -> (a -> b) -> (a -> b) -> SelectRing a -> Array b
mapEachIntoArray basicMutator focusedMutator selectedMutator selectedRing =
    selectedRing
        |> toArray
        |> Array.indexedMap
            (\index element ->
                if index == getFocusedIndex selectedRing then
                    focusedMutator element

                else if isSelectedAt index selectedRing then
                    selectedMutator element

                else
                    basicMutator element
            )
