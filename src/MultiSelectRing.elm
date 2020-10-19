module MultiSelectRing exposing
    ( MultiSelectRing
    , empty, singleton, fromList, fromArray
    , push, append, prepend
    , focusOn, focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
    , focusOnNextMatching, focusOnPreviousMatching, focusOnFirstMatching, focusOnLastMatching
    , selectAt, selectFirst, selectLast, selectFocused
    , selectAll, selectMany, selectManyMatching
    , deselectAt, deselectFirst, deselectLast, deselectFocused
    , deselectAll, deselectMany, deselectManyMatching
    , toggleAt, toggleFirst, toggleLast, toggleFocused
    , isEmpty
    , isNoneSelected, isAllSelected, isAnySelected, isSelectedAt
    , isFocusedAt, isFocusedMatching, isFocusedSelected
    , size, countSelected, countDeselected
    , get
    , getFirst, getLast
    , getFocused, getFocusedIndex
    , getSelected
    , set
    , toList, toArray
    , map
    , mapFocused, mapEachIntoList, mapEachIntoArray
    )

{-| A ring containing selectable elements

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and optionally selecting multiple elements at once.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


# Definition

@docs MultiSelectRing


# Construct a MultiSelectRing

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
@docs selectAll, selectMany, selectManyMatching


# Deselect an element

@docs deselectAt, deselectFirst, deselectLast, deselectFocused
@docs deselectAll, deselectMany, deselectManyMatching


# Toggle an element selection

@docs toggleAt, toggleFirst, toggleLast, toggleFocused


# Predicates

@docs isEmpty
@docs isNoneSelected, isAllSelected, isAnySelected, isSelectedAt
@docs isFocusedAt, isFocusedMatching, isFocusedSelected


# Accessors

@docs size, countSelected, countDeselected
@docs get
@docs getFirst, getLast
@docs getFocused, getFocusedIndex
@docs getSelected
@docs set


# Transform

@docs toList, toArray
@docs map
@docs mapFocused, mapEachIntoList, mapEachIntoArray

-}

import Array exposing (Array)
import Maybe
import Maybe.Extra as Maybe
import Set exposing (Set)



-- Definition


type MultiSelectRing a
    = MultiSelectRing
        { elements : Array a
        , focused : Int
        , selected : Set Int
        }



-- Construct a MultiSelectRing


{-| Construct an empty MultiSelectRing.
-}
empty : MultiSelectRing a
empty =
    MultiSelectRing
        { elements = Array.empty
        , focused = 0
        , selected = Set.empty
        }


{-| Construct a MultiSelectRing from a List (which might be empty).
-}
fromList : List a -> MultiSelectRing a
fromList list =
    MultiSelectRing
        { elements = Array.fromList list
        , focused = 0
        , selected = Set.empty
        }


{-| Construct a MultiSelectRing from an Array (which might be empty).
-}
fromArray : Array a -> MultiSelectRing a
fromArray array =
    MultiSelectRing
        { elements = array
        , focused = 0
        , selected = Set.empty
        }


{-| Construct a MultiSelectRing containing a single focused element.
-}
singleton : a -> MultiSelectRing a
singleton element =
    push element empty



-- Insert elements


{-| Insert an element at the end of the MultiSelectRing.
-}
push : a -> MultiSelectRing a -> MultiSelectRing a
push element (MultiSelectRing ring) =
    MultiSelectRing
        { ring
            | elements = Array.push element ring.elements
        }


{-| Insert several elements at the end of the MultiSelectRing.
-}
append : List a -> MultiSelectRing a -> MultiSelectRing a
append elements (MultiSelectRing ring) =
    MultiSelectRing
        { ring
            | elements = Array.append ring.elements (Array.fromList elements)
        }


{-| Insert several elements at the beginning of the MultiSelectRing.
Current focus and selected elements are maintained.
-}
prepend : List a -> MultiSelectRing a -> MultiSelectRing a
prepend elements (MultiSelectRing ring) =
    let
        shift =
            List.length elements
    in
    MultiSelectRing
        { ring
            | elements = Array.append (Array.fromList elements) ring.elements
            , focused = ring.focused + shift
            , selected = Set.map ((+) shift) ring.selected
        }



-- TODO Remove elements
-- removeAt, removeFirst, removeLast, removeFocused, removeSelected
--
--  Focus on an element


{-| Focus on the element in the ring at the provided index (modulo the ring size).
-}
focusOn : Int -> MultiSelectRing a -> MultiSelectRing a
focusOn index ((MultiSelectRing ring) as multiSelectRing) =
    MultiSelectRing
        { ring
            | focused = modBy (size multiSelectRing) index
        }


{-| Focus on the next element n the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : MultiSelectRing a -> MultiSelectRing a
focusOnNext ((MultiSelectRing ring) as multiSelectRing) =
    MultiSelectRing
        { ring
            | focused = modBy (size multiSelectRing) (ring.focused + 1)
        }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : MultiSelectRing a -> MultiSelectRing a
focusOnPrevious ((MultiSelectRing ring) as multiSelectRing) =
    MultiSelectRing
        { ring
            | focused = modBy (size multiSelectRing) (ring.focused - 1)
        }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : MultiSelectRing a -> MultiSelectRing a
focusOnFirst multiSelectRing =
    focusOn 0 multiSelectRing


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : MultiSelectRing a -> MultiSelectRing a
focusOnLast multiSelectRing =
    focusOn (size multiSelectRing - 1) multiSelectRing


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnNextMatching predicate ((MultiSelectRing ring) as multiSelectRing) =
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
        |> Maybe.map (\index -> focusOn index multiSelectRing)
        |> Maybe.withDefault multiSelectRing


{-| Focus on the previous element in the ring that matches the provided predicate.
-}
focusOnPreviousMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnPreviousMatching predicate ((MultiSelectRing ring) as multiSelectRing) =
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
        |> Maybe.map (\index -> focusOn index multiSelectRing)
        |> Maybe.withDefault multiSelectRing


{-| Focus on the first element in the ring that matches the provided predicate.
-}
focusOnFirstMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnFirstMatching predicate multiSelectRing =
    multiSelectRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index multiSelectRing)
        |> Maybe.withDefault multiSelectRing


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnLastMatching predicate multiSelectRing =
    multiSelectRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index multiSelectRing)
        |> Maybe.withDefault multiSelectRing



-- Select an element


{-| Select element at the provided index (modulo the ring size).
-}
selectAt : Int -> MultiSelectRing a -> MultiSelectRing a
selectAt index ((MultiSelectRing ring) as multiSelectRing) =
    let
        selectedIndex =
            modBy (size multiSelectRing) index
    in
    MultiSelectRing
        { ring
            | selected = Set.insert selectedIndex ring.selected
        }


{-| Select the first element of the ring.
-}
selectFirst : MultiSelectRing a -> MultiSelectRing a
selectFirst multiSelectRing =
    selectAt 0 multiSelectRing


{-| Select the last element of the ring.
-}
selectLast : MultiSelectRing a -> MultiSelectRing a
selectLast multiSelectRing =
    selectAt (size multiSelectRing - 1) multiSelectRing


{-| Select the currently focused element.
-}
selectFocused : MultiSelectRing a -> MultiSelectRing a
selectFocused multiSelectRing =
    selectAt (getFocusedIndex multiSelectRing) multiSelectRing


{-| Select all elements of the provided ring.
-}
selectAll : MultiSelectRing a -> MultiSelectRing a
selectAll ((MultiSelectRing ring) as multiSelectRing) =
    let
        allIndexes =
            List.range 0 (size multiSelectRing - 1)
                |> Set.fromList
    in
    MultiSelectRing
        { ring
            | selected = Set.union allIndexes ring.selected
        }


{-| Select all elements corresponding to the provided indexes (modulo the ring size).
-}
selectMany : List Int -> MultiSelectRing a -> MultiSelectRing a
selectMany indexes ((MultiSelectRing ring) as multiSelectRing) =
    let
        selectedIndexes =
            indexes
                |> List.map (\index -> modBy (size multiSelectRing) index)
                |> Set.fromList
    in
    MultiSelectRing
        { ring
            | selected = Set.union ring.selected selectedIndexes
        }


{-| Select all elements in the ring that match the provided predicate.
-}
selectManyMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
selectManyMatching predicate multiSelectRing =
    let
        matchingIndexes =
            multiSelectRing
                |> toArray
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
    in
    selectMany matchingIndexes multiSelectRing



-- Deselect an element


{-| Deselect element at the provided index (modulo the ring size).
-}
deselectAt : Int -> MultiSelectRing a -> MultiSelectRing a
deselectAt index ((MultiSelectRing ring) as multiSelectRing) =
    let
        deselectedIndex =
            modBy (size multiSelectRing) index
    in
    MultiSelectRing
        { ring
            | selected = Set.remove deselectedIndex ring.selected
        }


{-| Deselect the first element of the ring (if selected).
-}
deselectFirst : MultiSelectRing a -> MultiSelectRing a
deselectFirst multiSelectRing =
    deselectAt 0 multiSelectRing


{-| Deselect the last element of the ring (if selected).
-}
deselectLast : MultiSelectRing a -> MultiSelectRing a
deselectLast multiSelectRing =
    deselectAt (size multiSelectRing - 1) multiSelectRing


{-| Deselect the currently focused element.
-}
deselectFocused : MultiSelectRing a -> MultiSelectRing a
deselectFocused multiSelectRing =
    deselectAt (getFocusedIndex multiSelectRing) multiSelectRing


{-| Deselect all currently selected elements.
-}
deselectAll : MultiSelectRing a -> MultiSelectRing a
deselectAll (MultiSelectRing ring) =
    MultiSelectRing
        { ring
            | selected = Set.empty
        }


{-| Deselect all elements corresponding to the provided indexes (modulo the ring size).
-}
deselectMany : List Int -> MultiSelectRing a -> MultiSelectRing a
deselectMany indexes ((MultiSelectRing ring) as multiSelectRing) =
    let
        deselectedIndexes =
            indexes
                |> List.map (\index -> modBy (size multiSelectRing) index)
                |> Set.fromList
    in
    MultiSelectRing
        { ring
            | selected = Set.diff ring.selected deselectedIndexes
        }


{-| Deselect all elements in the ring that match the provided predicate.
-}
deselectManyMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
deselectManyMatching predicate multiSelectRing =
    let
        matchingIndexes =
            multiSelectRing
                |> toArray
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
    in
    deselectMany matchingIndexes multiSelectRing



-- Toggle an element selection


{-| Toggle selection of the element at the provided index (modulo the ring size).
-}
toggleAt : Int -> MultiSelectRing a -> MultiSelectRing a
toggleAt index multiSelectRing =
    let
        toggleIndex =
            modBy (size multiSelectRing) index
    in
    if isSelectedAt toggleIndex multiSelectRing then
        deselectAt toggleIndex multiSelectRing

    else
        selectAt toggleIndex multiSelectRing


{-| Toggle selection of the first element in the ring.
-}
toggleFirst : MultiSelectRing a -> MultiSelectRing a
toggleFirst multiSelectRing =
    toggleAt 0 multiSelectRing


{-| Toggle selection of the last element in the ring.
-}
toggleLast : MultiSelectRing a -> MultiSelectRing a
toggleLast multiSelectRing =
    toggleAt (size multiSelectRing - 1) multiSelectRing


{-| Toggle selection of the currently focused element.
-}
toggleFocused : MultiSelectRing a -> MultiSelectRing a
toggleFocused multiSelectRing =
    toggleAt (getFocusedIndex multiSelectRing) multiSelectRing



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : MultiSelectRing a -> Bool
isEmpty (MultiSelectRing ring) =
    Array.isEmpty ring.elements


{-| Indicate whether or not the ring has no selected elements.
-}
isNoneSelected : MultiSelectRing a -> Bool
isNoneSelected (MultiSelectRing ring) =
    Set.isEmpty ring.selected


{-| Indicate whether or not the ring has selected elements.
-}
isAnySelected : MultiSelectRing a -> Bool
isAnySelected multiSelectRing =
    countSelected multiSelectRing > 0


{-| Indicate whether or not all the ring elements have been selected.
-}
isAllSelected : MultiSelectRing a -> Bool
isAllSelected multiSelectRing =
    countSelected multiSelectRing == size multiSelectRing


{-| Indicate whether or not the element at the provided index (modulo the ring size) is selected.
-}
isSelectedAt : Int -> MultiSelectRing a -> Bool
isSelectedAt index ((MultiSelectRing ring) as multiSelectRing) =
    let
        selectedIndex =
            modBy (size multiSelectRing) index
    in
    Set.member selectedIndex ring.selected


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocusedAt : Int -> MultiSelectRing a -> Bool
isFocusedAt index multiSelectRing =
    getFocusedIndex multiSelectRing == modBy (size multiSelectRing) index


{-| Indicate whether or not the focused element matches the provided predicate.
-}
isFocusedMatching : (a -> Bool) -> MultiSelectRing a -> Bool
isFocusedMatching predicate multiSelectRing =
    getFocused multiSelectRing
        |> Maybe.map predicate
        |> Maybe.withDefault False


{-| Indicate whether or not the focused element is selected.
-}
isFocusedSelected : MultiSelectRing a -> Bool
isFocusedSelected multiSelectRing =
    isSelectedAt (getFocusedIndex multiSelectRing) multiSelectRing



-- Accessors


{-| Return the size of the ring.
-}
size : MultiSelectRing a -> Int
size (MultiSelectRing ring) =
    Array.length ring.elements


{-| Return the number of currently selected elements.
-}
countSelected : MultiSelectRing a -> Int
countSelected (MultiSelectRing ring) =
    Set.size ring.selected


{-| Return the number of currently deselected elements.
-}
countDeselected : MultiSelectRing a -> Int
countDeselected multiSelectRing =
    size multiSelectRing - countSelected multiSelectRing


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> MultiSelectRing a -> Maybe a
get index ((MultiSelectRing ring) as multiSelectRing) =
    let
        elementIndex =
            modBy (size multiSelectRing) index
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : MultiSelectRing a -> Maybe a
getFirst multiSelectRing =
    get 0 multiSelectRing


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : MultiSelectRing a -> Maybe a
getLast multiSelectRing =
    get (size multiSelectRing - 1) multiSelectRing


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : MultiSelectRing a -> Maybe a
getFocused multiSelectRing =
    get (getFocusedIndex multiSelectRing) multiSelectRing


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocusedIndex : MultiSelectRing a -> Int
getFocusedIndex (MultiSelectRing ring) =
    ring.focused


{-| Return the selected elements as a list. When none are selected, the empty list is returned.
-}
getSelected : MultiSelectRing a -> List a
getSelected multiSelectRing =
    multiSelectRing
        |> toArray
        |> Array.toIndexedList
        |> List.filterMap
            (\( index, element ) ->
                if isSelectedAt index multiSelectRing then
                    Just element

                else
                    Nothing
            )


{-| Set the element of the ring at the provided index (modulo the ring size) and returns an updated
MultiSelectRing without changing current focus and selection.
-}
set : Int -> a -> MultiSelectRing a -> MultiSelectRing a
set index element ((MultiSelectRing ring) as multiSelectRing) =
    let
        elementIndex =
            modBy (size multiSelectRing) index
    in
    MultiSelectRing
        { ring
            | elements =
                Array.set elementIndex element ring.elements
        }



-- Transform


{-| Create a list of elements from the MultiSelectRing.
-}
toList : MultiSelectRing a -> List a
toList (MultiSelectRing ring) =
    Array.toList ring.elements


{-| Create an array of elements from the MultiSelectRing.
-}
toArray : MultiSelectRing a -> Array a
toArray (MultiSelectRing ring) =
    ring.elements


map : (a -> b) -> MultiSelectRing a -> MultiSelectRing b
map mutator (MultiSelectRing ring) =
    MultiSelectRing
        { elements = Array.map mutator ring.elements
        , focused = ring.focused
        , selected = ring.selected
        }


mapFocused : (a -> b) -> MultiSelectRing a -> Maybe b
mapFocused mutator multiSelectRing =
    Maybe.map mutator (getFocused multiSelectRing)


mapEachIntoList : (a -> b) -> (a -> b) -> (a -> b) -> MultiSelectRing a -> List b
mapEachIntoList basicMutator focusedMutator selectedMutator multiSelectRing =
    Array.toList (mapEachIntoArray basicMutator focusedMutator selectedMutator multiSelectRing)


mapEachIntoArray : (a -> b) -> (a -> b) -> (a -> b) -> MultiSelectRing a -> Array b
mapEachIntoArray basicMutator focusedMutator selectedMutator multiSelectRing =
    multiSelectRing
        |> toArray
        |> Array.indexedMap
            (\index element ->
                if index == getFocusedIndex multiSelectRing then
                    focusedMutator element

                else if isSelectedAt index multiSelectRing then
                    selectedMutator element

                else
                    basicMutator element
            )
