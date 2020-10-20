module FocusRing exposing
    ( FocusRing
    , empty, singleton, fromList, fromArray
    , push, append, prepend
    , removeAt, removeFirst, removeLast, removeFocused
    , focusOn, focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
    , focusOnNextMatching, focusOnPreviousMatching, focusOnFirstMatching, focusOnLastMatching
    , isEmpty, isFocusedAt, isFocusedMatching
    , size
    , get, getFirst, getLast
    , getFocused, getFocusedIndex
    , set, setFocused
    , toList, toArray
    , map
    , mapFocused, mapEachIntoList, mapEachIntoArray
    )

{-| A ring containing a single focused element

This module provides a form of array that allows navigating through its elements as if it was
a ring and focusing on a single element.

Moving the cursor around allows to focus on a single element.
When the cursor is at the end of the array, focusing on the next element will return focus to the
first element of the array.
When the cursor is at the beginning of the array, focusing on the previous element will return focus
to the end of the array.


# Definition

@docs FocusRing


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

@docs isEmpty, isFocusedAt, isFocusedMatching


# Accessors

@docs size
@docs get, getFirst, getLast
@docs getFocused, getFocusedIndex
@docs set, setFocused


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


type FocusRing a
    = FocusRing
        { elements : Array a
        , focused : Int
        }



-- Construct a FocusRing


{-| Construct an empty FocusRing.
-}
empty : FocusRing a
empty =
    FocusRing
        { elements = Array.empty
        , focused = 0
        }


{-| Construct a FocusRing from a List (which might be empty).
-}
fromList : List a -> FocusRing a
fromList list =
    FocusRing
        { elements = Array.fromList list
        , focused = 0
        }


{-| Construct a FocusRing from an Array (which might be empty).
-}
fromArray : Array a -> FocusRing a
fromArray array =
    FocusRing
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
push element (FocusRing ring) =
    FocusRing
        { ring
            | elements = Array.push element ring.elements
        }


{-| Insert several elements at the end of the FocusRing.
-}
append : List a -> FocusRing a -> FocusRing a
append elements (FocusRing ring) =
    FocusRing
        { ring
            | elements = Array.append ring.elements (Array.fromList elements)
        }


{-| Insert several elements at the beginning of the FocusRing.
Current focus is maintained.
-}
prepend : List a -> FocusRing a -> FocusRing a
prepend elements (FocusRing ring) =
    let
        shift =
            List.length elements
    in
    FocusRing
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
removeAt index ((FocusRing ring) as focusRing) =
    let
        ringSize =
            size focusRing

        removedIndex =
            modBy ringSize index

        focusedIndex =
            if removedIndex == ringSize - 1 then
                modBy ringSize (ring.focused - 1)

            else
                ring.focused
    in
    FocusRing
        { ring
            | elements = Array.removeAt removedIndex ring.elements
            , focused = focusedIndex
        }


{-| Remove the first element of the ring.
If the focused element is removed, the focus is put on the next element.
-}
removeFirst : FocusRing a -> FocusRing a
removeFirst focusRing =
    removeAt 0 focusRing


{-| Remove the last element of the ring.
If the focused element is removed, the focus is put on the previous element.
-}
removeLast : FocusRing a -> FocusRing a
removeLast focusRing =
    removeAt (size focusRing - 1) focusRing


{-| Remove the focused element from the ring.

The focus is put on the next element, unless it is the last element of the ring. In that case,
the focus is put on previous element instead. This element also becomes the last one in the ring.

-}
removeFocused : FocusRing a -> FocusRing a
removeFocused focusRing =
    removeAt (getFocusedIndex focusRing) focusRing



-- Focus


{-| Focus on the element at the provided index (modulo the ring size).
-}
focusOn : Int -> FocusRing a -> FocusRing a
focusOn index ((FocusRing ring) as focusRing) =
    FocusRing
        { ring
            | focused = modBy (size focusRing) index
        }


{-| Focus on the next element in the ring, after the currently focused element.
When focus is on the last element, this results in focusing on the first element of the ring.
-}
focusOnNext : FocusRing a -> FocusRing a
focusOnNext ((FocusRing ring) as focusRing) =
    FocusRing
        { ring
            | focused = modBy (size focusRing) (ring.focused + 1)
        }


{-| Focus on the previous element in the ring, before to the currently focused element.
When focus is on the first element, this results in focusing on the last element of the ring.
-}
focusOnPrevious : FocusRing a -> FocusRing a
focusOnPrevious ((FocusRing ring) as focusRing) =
    FocusRing
        { ring
            | focused = modBy (size focusRing) (ring.focused - 1)
        }


{-| Focus on the first element of the ring. Empty rings remain unaffected.
-}
focusOnFirst : FocusRing a -> FocusRing a
focusOnFirst focusRing =
    focusOn 0 focusRing


{-| Focus on the last element of the ring. Empty rings remain unaffected.
-}
focusOnLast : FocusRing a -> FocusRing a
focusOnLast focusRing =
    focusOn (size focusRing - 1) focusRing


{-| Focus on the next element in the ring that matches the provided predicate.
-}
focusOnNextMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnNextMatching predicate ((FocusRing ring) as focusRing) =
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
        |> Maybe.map (\index -> focusOn index focusRing)
        |> Maybe.withDefault focusRing


{-| Focus on the previous element in the ring that matches the provided predicate.
-}
focusOnPreviousMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnPreviousMatching predicate ((FocusRing ring) as focusRing) =
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
        |> Maybe.map (\index -> focusOn index focusRing)
        |> Maybe.withDefault focusRing


{-| Focus on the first element in the ring that matches the provided predicate.
-}
focusOnFirstMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnFirstMatching predicate focusRing =
    focusRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index focusRing)
        |> Maybe.withDefault focusRing


{-| Focus on the last element in the ring that matches the provided predicate.
-}
focusOnLastMatching : (a -> Bool) -> FocusRing a -> FocusRing a
focusOnLastMatching predicate focusRing =
    focusRing
        |> toArray
        |> Array.toIndexedList
        |> List.filter (\( _, element ) -> predicate element)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( index, _ ) -> focusOn index focusRing)
        |> Maybe.withDefault focusRing



-- Predicates


{-| Indicate whether or not the ring is empty.
-}
isEmpty : FocusRing a -> Bool
isEmpty (FocusRing ring) =
    Array.isEmpty ring.elements


{-| Indicate whether or not the element at the provided index (modulo the ring size) is focused.
-}
isFocusedAt : Int -> FocusRing a -> Bool
isFocusedAt index focusRing =
    getFocusedIndex focusRing == modBy (size focusRing) index


{-| Indicate whether or not the focused element matches the provided predicate.
-}
isFocusedMatching : (a -> Bool) -> FocusRing a -> Bool
isFocusedMatching predicate focusRing =
    getFocused focusRing
        |> Maybe.map predicate
        |> Maybe.withDefault False



-- Accessors


{-| Return the size of the ring.
-}
size : FocusRing a -> Int
size (FocusRing ring) =
    Array.length ring.elements


{-| Return Just the element of the ring at the provided index or Nothing if the ring is empty.
-}
get : Int -> FocusRing a -> Maybe a
get index ((FocusRing ring) as focusRing) =
    let
        elementIndex =
            modBy (size focusRing) index
    in
    Array.get elementIndex ring.elements


{-| Return Just the first element of the ring or Nothing if the ring is empty.
-}
getFirst : FocusRing a -> Maybe a
getFirst focusRing =
    get 0 focusRing


{-| Return Just the last element of the ring or Nothing if the ring is empty.
-}
getLast : FocusRing a -> Maybe a
getLast focusRing =
    get (size focusRing - 1) focusRing


{-| Return Just the focused element of the ring or Nothing if the ring is empty.
-}
getFocused : FocusRing a -> Maybe a
getFocused focusRing =
    get (getFocusedIndex focusRing) focusRing


{-| Return the index of the focused element in the ring.
-}
getFocusedIndex : FocusRing a -> Int
getFocusedIndex (FocusRing ring) =
    ring.focused


{-| Set the element of the ring at the provided index (modulo the ring size) and return an updated
FocusRing without changing current focus state.
-}
set : Int -> a -> FocusRing a -> FocusRing a
set index element ((FocusRing ring) as focusRing) =
    let
        elementIndex =
            modBy (size focusRing) index
    in
    FocusRing
        { ring
            | elements =
                Array.set elementIndex element ring.elements
        }


{-| Set the focused element of the ring and return an updated FocusRing.
-}
setFocused : a -> FocusRing a -> FocusRing a
setFocused element focusRing =
    focusRing
        |> set (getFocusedIndex focusRing) element



-- Transform


{-| Create a list of elements from the FocusRing.
-}
toList : FocusRing a -> List a
toList (FocusRing ring) =
    Array.toList ring.elements


{-| Create an array of elements from the FocusRing.
-}
toArray : FocusRing a -> Array a
toArray (FocusRing ring) =
    ring.elements


map : (a -> b) -> FocusRing a -> FocusRing b
map mutator (FocusRing ring) =
    FocusRing
        { elements = Array.map mutator ring.elements
        , focused = ring.focused
        }


mapFocused : (a -> b) -> FocusRing a -> Maybe b
mapFocused mutator focusRing =
    Maybe.map mutator (getFocused focusRing)


mapEachIntoList : (a -> b) -> (a -> b) -> FocusRing a -> List b
mapEachIntoList basicMutator focusedMutator focusRing =
    Array.toList (mapEachIntoArray basicMutator focusedMutator focusRing)


mapEachIntoArray : (a -> b) -> (a -> b) -> FocusRing a -> Array b
mapEachIntoArray basicMutator focusedMutator focusRing =
    focusRing
        |> toArray
        |> Array.indexedMap
            (\index element ->
                if index == getFocusedIndex focusRing then
                    focusedMutator element

                else
                    basicMutator element
            )
