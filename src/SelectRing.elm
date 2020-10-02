module SelectRing exposing (..)

{-| A ring containing at most a single selected element

This module provides a form of array that allows navigating through its elements (or focusing)
as if it was a ring and selecting at most a single element from it.

Moving the cursor around allows to focus on a single element (e.g. to select it if wanted).
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


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

@docs isEmpty, isNoneSelected, isAnySelected, isSelected, isFocused


# Accessors

@docs size, get, getFirst, getLast, getFocused, getSelected


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


type alias SelectRing a =
    { elements : Array a
    , focused : Int
    , selected : Maybe Int
    }



-- Construct a SelectRing


{-| Construct an empty SelectRing.
-}
empty : SelectRing a
empty =
    { elements = Array.empty
    , focused = 0
    , selected = Nothing
    }


{-| Construct a SelectRing from a List (which might be empty).
-}
fromList : List a -> SelectRing a
fromList list =
    { elements = Array.fromList list
    , focused = 0
    , selected = Nothing
    }


{-| Construct a SelectRing from an Array (which might be empty).
-}
fromArray : Array a -> SelectRing a
fromArray array =
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
push element ring =
    { ring
        | elements = Array.push element ring.elements
    }


{-| Insert several elements at the end of the SelectRing.
-}
append : List a -> SelectRing a -> SelectRing a
append elements ring =
    { ring
        | elements = Array.append ring.elements (Array.fromList elements)
    }


{-| Insert several elements at the beginning of the SelectRing.
Current focus and selected elements are maintained.
-}
prepend : List a -> SelectRing a -> SelectRing a
prepend elements ring =
    let
        shift =
            List.length elements
    in
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
removeAt index ring =
    let
        removedIndex =
            modBy index (size ring)

        focusedIndex =
            if removedIndex == size ring - 1 then
                modBy (ring.focused - 1) (size ring)

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
removeFirst ring =
    removeAt 0 ring


{-| Remove the last element of the ring.
If the focused element is removed, the focus is put on the previous element.
If the selected element is removed, then Nothing is selected anymore.
-}
removeLast : SelectRing a -> SelectRing a
removeLast ring =
    removeAt (size ring - 1) ring


{-| Remove the focused element from the ring.

The focus is put on the next element, unless it is the last element of the ring. In that case,
the focus is put on previous element instead. This element also becomes the last one in the ring.

If the selected element is removed, then Nothing is selected anymore.

-}
removeFocused : SelectRing a -> SelectRing a
removeFocused ring =
    removeAt ring.focused ring


{-| Remove the selected element from the ring. Nothing is selected anymore.

If the focused element is removed, the focus is put on the next element, unless it is the last
element of the ring. In that case, the focus is put on previous element instead. This element also
becomes the last one in the ring.

-}
removeSelected : SelectRing a -> SelectRing a
removeSelected ring =
    ring.selected
        |> Maybe.map (\index -> removeAt index ring)
        |> Maybe.withDefault ring



-- Focus


{-| Focus on the element at the provided index (modulo the ring size).
-}
focusOn : Int -> SelectRing a -> SelectRing a
focusOn index ring =
    { ring
        | focused = modBy index (size ring)
    }


{-| Focus on the next element in the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : SelectRing a -> SelectRing a
focusOnNext ring =
    { ring
        | focused = modBy (ring.focused + 1) (size ring)
    }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : SelectRing a -> SelectRing a
focusOnPrevious ring =
    { ring
        | focused = modBy (ring.focused - 1) (size ring)
    }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : SelectRing a -> SelectRing a
focusOnFirst ring =
    focusOn 0 ring


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : SelectRing a -> SelectRing a
focusOnLast ring =
    focusOn (size ring - 1) ring


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> SelectRing a -> SelectRing a
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
focusOnPreviousMatching : (a -> Bool) -> SelectRing a -> SelectRing a
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
focusOnFirstMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnFirstMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> SelectRing a -> SelectRing a
focusOnLastMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring



-- Select an element


{-| Select the element at the provided index (modulo the ring size).
This replaces the currently selected element.
-}
selectAt : Int -> SelectRing a -> SelectRing a
selectAt index ring =
    let
        selectedIndex =
            modBy index (size ring)
    in
    { ring
        | selected = Just selectedIndex
    }


{-| Select the first element of the ring. This replaces the currently selected element.
-}
selectFirst : SelectRing a -> SelectRing a
selectFirst ring =
    selectAt 0 ring


{-| Select the last element of the ring. This replaces the currently selected element.
-}
selectLast : SelectRing a -> SelectRing a
selectLast ring =
    selectAt (size ring - 1) ring


{-| Select the currently focused element. This replaces the currently selected element.
-}
selectFocused : SelectRing a -> SelectRing a
selectFocused ring =
    selectAt ring.focused ring


{-| Select the first element in the ring that matches the provided predicate.
This replaces the currently selected element.
-}
selectFirstMatching : (a -> Bool) -> SelectRing a -> SelectRing a
selectFirstMatching predicate ring =
    let
        matchingIndex =
            ring.elements
                |> Array.toIndexedList
                |> List.filter (\( _, element ) -> predicate element)
                |> List.map Tuple.first
                |> List.head
    in
    matchingIndex
        |> Maybe.map (\index -> selectAt index ring)
        |> Maybe.withDefault ring



-- Deselect an element


{-| Deselect the currently selected element.
-}
clearSelected : SelectRing a -> SelectRing a
clearSelected ring =
    { ring
        | selected = Nothing
    }


{-| Deselect element at the provided index (modulo the ring size).
If the element is already deselected, the same SelectRing is returned unchanged.
-}
deselectAt : Int -> SelectRing a -> SelectRing a
deselectAt index ring =
    let
        deselectedIndex =
            modBy index (size ring)
    in
    if ring.selected == Just deselectedIndex then
        clearSelected ring

    else
        ring


{-| Deselect the first element of the ring (if selected).
-}
deselectFirst : SelectRing a -> SelectRing a
deselectFirst ring =
    deselectAt 0 ring


{-| Deselect the last element of the ring (if selected).
-}
deselectLast : SelectRing a -> SelectRing a
deselectLast ring =
    deselectAt (size ring - 1) ring


{-| Deselect the currently focused element.
-}
deselectFocused : SelectRing a -> SelectRing a
deselectFocused ring =
    deselectAt ring.focused ring


{-| Deselect currently selected element if it matches the provided predicate.
-}
deselectMatching : (a -> Bool) -> SelectRing a -> SelectRing a
deselectMatching predicate ring =
    getSelected ring
        |> Maybe.filter (\selected -> predicate selected)
        |> Maybe.map (\_ -> clearSelected ring)
        |> Maybe.withDefault ring



-- Toggle an element selection


{-| Toggle selection of the element at the provided index (modulo the ring size).
-}
toggleAt : Int -> SelectRing a -> SelectRing a
toggleAt index ring =
    if isSelected index ring then
        deselectAt index ring

    else
        selectAt index ring


{-| Toggle selection of the first element in the ring.
-}
toggleFirst : SelectRing a -> SelectRing a
toggleFirst ring =
    toggleAt 0 ring


{-| Toggle selection of the last element in the ring.
-}
toggleLast : SelectRing a -> SelectRing a
toggleLast ring =
    toggleAt (size ring - 1) ring


{-| Toggle selection of the currently focused element.
-}
toggleFocused : SelectRing a -> SelectRing a
toggleFocused ring =
    toggleAt ring.focused ring



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : SelectRing a -> Bool
isEmpty ring =
    Array.isEmpty ring.elements


{-| Indicate whether or not the ring has no selected element.
-}
isNoneSelected : SelectRing a -> Bool
isNoneSelected ring =
    Maybe.isNothing ring.selected


{-| Indicate whether or not the ring has a selected element.
-}
isAnySelected : SelectRing a -> Bool
isAnySelected ring =
    Maybe.isJust ring.selected


{-| Indicate whether or not the element at the provided index (modulo the ring size) is selected.
-}
isSelected : Int -> SelectRing a -> Bool
isSelected index ring =
    let
        selectedIndex =
            modBy index (size ring)
    in
    ring.selected == Just selectedIndex


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocused : Int -> SelectRing a -> Bool
isFocused index ring =
    ring.focused == modBy index (size ring)



-- Accessors


{-| Return the size of the ring.
-}
size : SelectRing a -> Int
size ring =
    Array.length ring.elements


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> SelectRing a -> Maybe a
get index ring =
    let
        elementIndex =
            modBy index (size ring)
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : SelectRing a -> Maybe a
getFirst ring =
    get 0 ring


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : SelectRing a -> Maybe a
getLast ring =
    get (size ring - 1) ring


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : SelectRing a -> Maybe a
getFocused ring =
    get ring.focused ring


{-| Return Just the selected element. When none are selected, Nothing is returned instead.
-}
getSelected : SelectRing a -> Maybe a
getSelected ring =
    ring.selected
        |> Maybe.andThen (\index -> get index ring)



-- Transform


{-| Create a list of elements from the SelectRing.
-}
toList : SelectRing a -> List a
toList ring =
    Array.toList ring.elements


{-| Create an array of elements from the SelectRing.
-}
toArray : SelectRing a -> Array a
toArray ring =
    ring.elements


map : (a -> b) -> SelectRing a -> SelectRing b
map mutator ring =
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
