module MultiSelectRing exposing (..)

{-| A ring containing selectable elements

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and optionally selecting multiple elements at once.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


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

@docs isEmpty, isNoneSelected, isAnySelected, isSelected, isFocused


# Accessors

@docs size, get, getFirst, getLast, getFocused, getSelected


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


type alias MultiSelectRing a =
    { elements : Array a
    , focused : Int
    , selected : Set Int
    }



-- Construct a MultiSelectRing


{-| Construct an empty MultiSelectRing.
-}
empty : MultiSelectRing a
empty =
    { elements = Array.empty
    , focused = 0
    , selected = Set.empty
    }


{-| Construct a MultiSelectRing from a List (which might be empty).
-}
fromList : List a -> MultiSelectRing a
fromList list =
    { elements = Array.fromList list
    , focused = 0
    , selected = Set.empty
    }


{-| Construct a MultiSelectRing from an Array (which might be empty).
-}
fromArray : Array a -> MultiSelectRing a
fromArray array =
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
push element ring =
    { ring
        | elements = Array.push element ring.elements
    }


{-| Insert several elements at the end of the MultiSelectRing.
-}
append : List a -> MultiSelectRing a -> MultiSelectRing a
append elements ring =
    { ring
        | elements = Array.append ring.elements (Array.fromList elements)
    }


{-| Insert several elements at the beginning of the MultiSelectRing.
Current focus and selected elements are maintained.
-}
prepend : List a -> MultiSelectRing a -> MultiSelectRing a
prepend elements ring =
    let
        shift =
            List.length elements
    in
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
focusOn index ring =
    { ring
        | focused = modBy (size ring) index
    }


{-| Focus on the next element n the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : MultiSelectRing a -> MultiSelectRing a
focusOnNext ring =
    { ring
        | focused = modBy (size ring) (ring.focused + 1)
    }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : MultiSelectRing a -> MultiSelectRing a
focusOnPrevious ring =
    { ring
        | focused = modBy (size ring) (ring.focused - 1)
    }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : MultiSelectRing a -> MultiSelectRing a
focusOnFirst ring =
    focusOn 0 ring


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : MultiSelectRing a -> MultiSelectRing a
focusOnLast ring =
    focusOn (size ring - 1) ring


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnNextMatching predicate ring =
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
        |> Maybe.map (\index -> focusOn index ring)
        |> Maybe.withDefault ring


{-| Focus on the previous element in the ring that matches the provided predicate.
-}
focusOnPreviousMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnPreviousMatching predicate ring =
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
        |> Maybe.map (\index -> focusOn index ring)
        |> Maybe.withDefault ring


{-| Focus on the first element in the ring that matches the provided predicate.
-}
focusOnFirstMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnFirstMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
focusOnLastMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring



-- Select an element


{-| Select element at the provided index (modulo the ring size).
-}
selectAt : Int -> MultiSelectRing a -> MultiSelectRing a
selectAt index ring =
    let
        selectedIndex =
            modBy (size ring) index
    in
    { ring
        | selected = Set.insert selectedIndex ring.selected
    }


{-| Select the first element of the ring.
-}
selectFirst : MultiSelectRing a -> MultiSelectRing a
selectFirst ring =
    selectAt 0 ring


{-| Select the last element of the ring.
-}
selectLast : MultiSelectRing a -> MultiSelectRing a
selectLast ring =
    selectAt (size ring - 1) ring


{-| Select the currently focused element.
-}
selectFocused : MultiSelectRing a -> MultiSelectRing a
selectFocused ring =
    selectAt ring.focused ring


{-| Select all elements of the provided ring.
-}
selectAll : MultiSelectRing a -> MultiSelectRing a
selectAll ring =
    let
        allIndexes =
            List.range 0 (size ring - 1)
                |> Set.fromList
    in
    { ring
        | selected = Set.union allIndexes ring.selected
    }


{-| Select all elements corresponding to the provided indexes (modulo the ring size).
-}
selectMany : List Int -> MultiSelectRing a -> MultiSelectRing a
selectMany indexes ring =
    let
        selectedIndexes =
            indexes
                |> List.map (\index -> modBy (size ring) index)
                |> Set.fromList
    in
    { ring
        | selected = Set.union ring.selected selectedIndexes
    }


{-| Select all elements in the ring that match the provided predicate.
-}
selectManyMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
selectManyMatching predicate ring =
    let
        matchingIndexes =
            ring.elements
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
    in
    selectMany matchingIndexes ring



-- Deselect an element


{-| Deselect element at the provided index (modulo the ring size).
-}
deselectAt : Int -> MultiSelectRing a -> MultiSelectRing a
deselectAt index ring =
    let
        deselectedIndex =
            modBy (size ring) index
    in
    { ring
        | selected = Set.remove deselectedIndex ring.selected
    }


{-| Deselect the first element of the ring (if selected).
-}
deselectFirst : MultiSelectRing a -> MultiSelectRing a
deselectFirst ring =
    deselectAt 0 ring


{-| Deselect the last element of the ring (if selected).
-}
deselectLast : MultiSelectRing a -> MultiSelectRing a
deselectLast ring =
    deselectAt (size ring - 1) ring


{-| Deselect the currently focused element.
-}
deselectFocused : MultiSelectRing a -> MultiSelectRing a
deselectFocused ring =
    deselectAt ring.focused ring


{-| Deselect all currently selected elements.
-}
deselectAll : MultiSelectRing a -> MultiSelectRing a
deselectAll ring =
    { ring
        | selected = Set.empty
    }


{-| Deselect all elements corresponding to the provided indexes (modulo the ring size).
-}
deselectMany : List Int -> MultiSelectRing a -> MultiSelectRing a
deselectMany indexes ring =
    let
        deselectedIndexes =
            indexes
                |> List.map (\index -> modBy (size ring) index)
                |> Set.fromList
    in
    { ring
        | selected = Set.diff ring.selected deselectedIndexes
    }


{-| Deselect all elements in the ring that match the provided predicate.
-}
deselectManyMatching : (a -> Bool) -> MultiSelectRing a -> MultiSelectRing a
deselectManyMatching predicate ring =
    let
        matchingIndexes =
            ring.elements
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
    in
    deselectMany matchingIndexes ring



-- Toggle an element selection


{-| Toggle selection of the element at the provided index (modulo the ring size).
-}
toggleAt : Int -> MultiSelectRing a -> MultiSelectRing a
toggleAt index ring =
    let
        toggleIndex =
            modBy (size ring) index
    in
    if isSelected toggleIndex ring then
        deselectAt toggleIndex ring

    else
        selectAt toggleIndex ring


{-| Toggle selection of the first element in the ring.
-}
toggleFirst : MultiSelectRing a -> MultiSelectRing a
toggleFirst ring =
    toggleAt 0 ring


{-| Toggle selection of the last element in the ring.
-}
toggleLast : MultiSelectRing a -> MultiSelectRing a
toggleLast ring =
    toggleAt (size ring - 1) ring


{-| Toggle selection of the currently focused element.
-}
toggleFocused : MultiSelectRing a -> MultiSelectRing a
toggleFocused ring =
    toggleAt ring.focused ring



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : MultiSelectRing a -> Bool
isEmpty ring =
    Array.isEmpty ring.elements


{-| Indicate whether or not the ring has no selected elements.
-}
isNoneSelected : MultiSelectRing a -> Bool
isNoneSelected ring =
    Set.isEmpty ring.selected


{-| Indicate whether or not the ring has selected elements.
-}
isAnySelected : MultiSelectRing a -> Bool
isAnySelected ring =
    Set.size ring.selected > 0


{-| Indicate whether or not the element at the provided index (modulo the ring size) is selected.
-}
isSelected : Int -> MultiSelectRing a -> Bool
isSelected index ring =
    let
        selectedIndex =
            modBy (size ring) index
    in
    Set.member selectedIndex ring.selected


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocused : Int -> MultiSelectRing a -> Bool
isFocused index ring =
    ring.focused == modBy (size ring) index



-- Accessors


{-| Return the size of the ring.
-}
size : MultiSelectRing a -> Int
size ring =
    Array.length ring.elements


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> MultiSelectRing a -> Maybe a
get index ring =
    let
        elementIndex =
            modBy (size ring) index
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : MultiSelectRing a -> Maybe a
getFirst ring =
    get 0 ring


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : MultiSelectRing a -> Maybe a
getLast ring =
    get (size ring - 1) ring


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : MultiSelectRing a -> Maybe a
getFocused ring =
    get ring.focused ring


{-| Return the selected elements as a list. When none are selected, the empty list is returned.
-}
getSelected : MultiSelectRing a -> List a
getSelected ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filterMap
            (\( index, element ) ->
                if isSelected index ring then
                    Just element

                else
                    Nothing
            )



-- Transform


{-| Create a list of elements from the MultiSelectRing.
-}
toList : MultiSelectRing a -> List a
toList ring =
    Array.toList ring.elements


{-| Create an array of elements from the MultiSelectRing.
-}
toArray : MultiSelectRing a -> Array a
toArray ring =
    ring.elements


map : (a -> b) -> MultiSelectRing a -> MultiSelectRing b
map mutator ring =
    { elements = Array.map mutator ring.elements
    , focused = ring.focused
    , selected = ring.selected
    }


mapFocused : (a -> b) -> MultiSelectRing a -> Maybe b
mapFocused mutator ring =
    Maybe.map mutator (getFocused ring)


mapEachIntoList : (a -> b) -> (a -> b) -> (a -> b) -> MultiSelectRing a -> List b
mapEachIntoList basicMutator focusedMutator selectedMutator ring =
    Array.toList (mapEachIntoArray basicMutator focusedMutator selectedMutator ring)


mapEachIntoArray : (a -> b) -> (a -> b) -> (a -> b) -> MultiSelectRing a -> Array b
mapEachIntoArray basicMutator focusedMutator selectedMutator ring =
    ring.elements
        |> Array.indexedMap
            (\index element ->
                if index == ring.focused then
                    focusedMutator element

                else if isSelected index ring then
                    selectedMutator element

                else
                    basicMutator element
            )
