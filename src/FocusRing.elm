module FocusRing exposing (..)

{-| A ring containing a single focused element

This module provides a form of array that allows navigating through its elements as if it was
a ring and focusing on a single element.

Moving the cursor around allows to focus on a single element.
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


# Construct a FocusRing

@docs empty, singleton, fromList, fromArray


# Insert elements

@docs push, append, prepend


# Remove elements

@docs removeAt, removeFirst, removeLast, removeFocused


# Focus on an element

@docs focusOn, focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
@docs focusOnNextMatching, focusOnPreviousMatching, focusOnFirstMatching, focusOnLastMatching


# Predicates

@docs isEmpty, isFocused


# Accessors

@docs size, get, getFirst, getLast, getFocused


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


type alias FocusRing a =
    { elements : Array a
    , focused : Int
    }



-- Construct a FocusRing


{-| Construct an empty FocusRing.
-}
empty : FocusRing a
empty =
    { elements = Array.empty
    , focused = 0
    }


{-| Construct a FocusRing from a List (which might be empty).
-}
fromList : List a -> FocusRing a
fromList list =
    { elements = Array.fromList list
    , focused = 0
    }


{-| Construct a FocusRing from an Array (which might be empty).
-}
fromArray : Array a -> FocusRing a
fromArray array =
    { elements = array
    , focused = 0
    }


{-| Construct a FocusRing containing a single focused element.
-}
singleton : a -> FocusRing a
singleton element =
    push element empty



-- Insert elements


{-| Insert an element at the end of the FocusRing.
-}
push : a -> FocusRing a -> FocusRing a
push element ring =
    { ring
        | elements = Array.push element ring.elements
    }


{-| Insert several elements at the end of the FocusRing.
-}
append : List a -> FocusRing a -> FocusRing a
append elements ring =
    { ring
        | elements = Array.append ring.elements (Array.fromList elements)
    }


{-| Insert several elements at the beginning of the FocusRing.
Current focus is maintained.
-}
prepend : List a -> FocusRing a -> FocusRing a
prepend elements ring =
    let
        shift =
            List.length elements
    in
    { ring
        | elements = Array.append (Array.fromList elements) ring.elements
        , focused = ring.focused + shift
    }



-- Remove elements


{-| Remove the element at the provided index (modulo the ring size).

If the focused element is removed, the focus is put on the next element, unless it is the last
element of the ring. In that case, the focus is put on previous element instead. This element also
becomes the last one in the ring.

-}
removeAt : Int -> FocusRing a -> FocusRing a
removeAt index ring =
    let
        removedIndex =
            modBy (size ring) index

        focusedIndex =
            if removedIndex == size ring - 1 then
                modBy (size ring) (ring.focused - 1)

            else
                ring.focused
    in
    { ring
        | elements = Array.removeAt removedIndex ring.elements
        , focused = focusedIndex
    }


{-| Remove the first element of the ring.
If the focused element is removed, the focus is put on the next element.
-}
removeFirst : FocusRing a -> FocusRing a
removeFirst ring =
    removeAt 0 ring


{-| Remove the last element of the ring.
If the focused element is removed, the focus is put on the previous element.
-}
removeLast : FocusRing a -> FocusRing a
removeLast ring =
    removeAt (size ring - 1) ring


{-| Remove the focused element from the ring.

The focus is put on the next element, unless it is the last element of the ring. In that case,
the focus is put on previous element instead. This element also becomes the last one in the ring.

-}
removeFocused : FocusRing a -> FocusRing a
removeFocused ring =
    removeAt ring.focused ring



-- Focus


{-| Focus on the element at the provided index (modulo the ring size).
-}
focusOn : Int -> FocusRing a -> FocusRing a
focusOn index ring =
    { ring
        | focused = modBy (size ring) index
    }


{-| Focus on the next element in the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : FocusRing a -> FocusRing a
focusOnNext ring =
    { ring
        | focused = modBy (size ring) (ring.focused + 1)
    }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : FocusRing a -> FocusRing a
focusOnPrevious ring =
    { ring
        | focused = modBy (size ring) (ring.focused - 1)
    }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : FocusRing a -> FocusRing a
focusOnFirst ring =
    focusOn 0 ring


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : FocusRing a -> FocusRing a
focusOnLast ring =
    focusOn (size ring - 1) ring


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> FocusRing a -> FocusRing a
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
focusOnPreviousMatching : (a -> Bool) -> FocusRing a -> FocusRing a
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
focusOnFirstMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnFirstMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnLastMatching predicate ring =
    ring.elements
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index ring)
        |> Maybe.withDefault ring



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : FocusRing a -> Bool
isEmpty ring =
    Array.isEmpty ring.elements


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocused : Int -> FocusRing a -> Bool
isFocused index ring =
    ring.focused == modBy (size ring) index



-- Accessors


{-| Return the size of the ring.
-}
size : FocusRing a -> Int
size ring =
    Array.length ring.elements


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> FocusRing a -> Maybe a
get index ring =
    let
        elementIndex =
            modBy (size ring) index
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : FocusRing a -> Maybe a
getFirst ring =
    get 0 ring


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : FocusRing a -> Maybe a
getLast ring =
    get (size ring - 1) ring


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : FocusRing a -> Maybe a
getFocused ring =
    get ring.focused ring



-- Transform


{-| Create a list of elements from the FocusRing.
-}
toList : FocusRing a -> List a
toList ring =
    Array.toList ring.elements


{-| Create an array of elements from the FocusRing.
-}
toArray : FocusRing a -> Array a
toArray ring =
    ring.elements


map : (a -> b) -> FocusRing a -> FocusRing b
map mutator ring =
    { elements = Array.map mutator ring.elements
    , focused = ring.focused
    }


mapFocused : (a -> b) -> FocusRing a -> Maybe b
mapFocused mutator ring =
    Maybe.map mutator (getFocused ring)


mapEachIntoList : (a -> b) -> (a -> b) -> FocusRing a -> List b
mapEachIntoList basicMutator focusedMutator ring =
    Array.toList (mapEachIntoArray basicMutator focusedMutator ring)


mapEachIntoArray : (a -> b) -> (a -> b) -> FocusRing a -> Array b
mapEachIntoArray basicMutator focusedMutator ring =
    ring.elements
        |> Array.indexedMap
            (\index element ->
                if index == ring.focused then
                    focusedMutator element

                else
                    basicMutator element
            )
