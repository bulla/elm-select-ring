module ZipperRing exposing
    ( singleton, fromList, fromListWithDefault
    , append, prepend
    , focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
    , focusOnNextMatching
    , getFocused
    , toList, map, mapFocused
    , ZipperRing
    )

{-| A zipper ring
This **experimental** module provides a form of non-empty focused ring that uses a zipper list as its
internal structure instead of an array.

Moving the focus around allows to focus on a single element.
When the focus is at the end, focusing on the next element will return focus to the first element.
When the focus is at the beginning, focusing on the previous element will return focus to the last
element.

As an example:
ZipperRing.fromListWithDefault 1 [1, 2, 3]
|> ZipperRing.focusOnNext
|> ZipperRing.focusOnNext
|> ZipperRing.focusOnNext
|> ZipperRing.getFocused
-- 1
ZipperRing.fromListWithDefault 4 [1, 2, 3]
|> ZipperRing.prepend 5
|> ZipperRing.focusOnFirst
|> ZipperRing.getFocused
-- 5
ZipperRing.fromListWithDefault 4 [1, 2, 3]
|> ZipperRing.focusOnNext
|> ZipperRing.mapFocused ((+) 1)
|> ZipperRing.getFocused
-- 3

This module has been essentially inspired by the work of maorleger and ported over to the latest
version of Elm.
See <https://raw.githubusercontent.com/maorleger/elm-infinite-zipper/master/src/List/InfiniteZipper.elm>
for more details on the initial implementation.


# Construct a ZipperRing

@docs singleton, fromList, fromListWithDefault


# Insert elements

@docs push, append, prepend


# Focus on an element

@docs focusOnNext, focusOnPrevious, focusOnFirst, focusOnLast
@docs focusOnNextMatching, focusOnFirstMatching


# Accessors

@docs size, getFocused


# Transform

@docs toList, map, mapFocused

-}

import List exposing (reverse)



-- Definition


type ZipperRing a
    = ZipperRing (List a) a (List a)



-- Construct a ZipperRing


{-| Construct an ZipperRing from a List. If the list is empty, Nothing is returned.
-}
fromList : List a -> Maybe (ZipperRing a)
fromList list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (ZipperRing [] x xs)


{-| Construct an ZipperRing from a List. If the List is empty, create a singleton ZipperRing with
a focus on the provided default value.
-}
fromListWithDefault : a -> List a -> ZipperRing a
fromListWithDefault default list =
    fromList list
        |> Maybe.withDefault (singleton default)


{-| Create an ZipperRing with a single focused element.
-}
singleton : a -> ZipperRing a
singleton x =
    ZipperRing [] x []



-- Insert elements


{-| Insert an element to the end of the ZipperRing, maintaining current focus. Same as append.
-}
push : a -> ZipperRing a -> ZipperRing a
push element zipper =
    append element zipper


{-| Insert an element to the end of the ZipperRing, maintaining current focus. Same as push.
-}
append : a -> ZipperRing a -> ZipperRing a
append element (ZipperRing left focused right) =
    ZipperRing left focused (right ++ [ element ])


{-| Insert an element to the beginning of the ZipperRing, maintaining current focus.
-}
prepend : a -> ZipperRing a -> ZipperRing a
prepend element (ZipperRing left focused right) =
    ZipperRing (left ++ [ element ]) focused right



-- Focus on an element


{-| Focus on the first element of the ZipperRing.
-}
focusOnFirst : ZipperRing a -> ZipperRing a
focusOnFirst (ZipperRing left focused right) =
    case reverse left of
        [] ->
            ZipperRing left focused right

        x :: xs ->
            ZipperRing [] x (xs ++ [ focused ] ++ right)


{-| Focus to the last element of the ZipperRing.
-}
focusOnLast : ZipperRing a -> ZipperRing a
focusOnLast (ZipperRing left focused right) =
    case reverse right of
        [] ->
            ZipperRing left focused right

        x :: xs ->
            ZipperRing (xs ++ [ focused ] ++ left) x []


{-| Focus on the next element in the ring.
If focus is currently on the last element of the ring, the focus is put on the first element.
-}
focusOnNext : ZipperRing a -> ZipperRing a
focusOnNext ((ZipperRing left focused right) as zipper) =
    case right of
        [] ->
            focusOnFirst zipper

        x :: xs ->
            ZipperRing (focused :: left) x xs


{-| Focus on the previous element in the ring.
If focus is currently on the last element of the ring, the focus is put on the first element.
-}
focusOnPrevious : ZipperRing a -> ZipperRing a
focusOnPrevious ((ZipperRing left focused right) as zipper) =
    case left of
        [] ->
            focusOnLast zipper

        x :: xs ->
            ZipperRing xs x (focused :: right)


{-| Return a ZipperRing focused on the first element that matches the provided predicate.
Start at the beginning of the ZipperRing and search forwards until the end.
-}
focusOnFirstMatching : (a -> Bool) -> ZipperRing a -> Maybe (ZipperRing a)
focusOnFirstMatching predicate zipper =
    find predicate (focusOnFirst zipper)


find : (a -> Bool) -> ZipperRing a -> Maybe (ZipperRing a)
find pred ((ZipperRing _ _ right) as zipper) =
    if pred (getFocused zipper) then
        Just zipper

    else if List.isEmpty right then
        Nothing

    else
        find pred <| focusOnNext zipper


{-| Return a ZipperRing focused on the next element that matches the provided predicate.
Start at the next element from the current focus and loop around (excluding the focused element).
-}
focusOnNextMatching : (a -> Bool) -> ZipperRing a -> Maybe (ZipperRing a)
focusOnNextMatching predicate zipper =
    let
        search currentZipper =
            if equals currentZipper zipper then
                Nothing

            else if predicate (getFocused currentZipper) then
                Just currentZipper

            else
                search (focusOnNext currentZipper)
    in
    search (focusOnNext zipper)


{-| Return whether or not the two provided ZipperRings are identical.
-}
equals : ZipperRing a -> ZipperRing a -> Bool
equals (ZipperRing left1 focused1 right1) (ZipperRing left2 focused2 right2) =
    left1 == left2 && focused1 == focused2 && right1 == right2



-- Accessors


{-| Return the size of the ZipperRing.
-}
size : ZipperRing a -> Int
size (ZipperRing left _ right) =
    List.length left + 1 + List.length right


{-| Get the element that the ZipperRing is currently focused on.
-}
getFocused : ZipperRing a -> a
getFocused (ZipperRing _ focused _) =
    focused



-- Transform


{-| Create a List from the ZipperRing elements.
-}
toList : ZipperRing a -> List a
toList (ZipperRing left focused right) =
    reverse left ++ [ focused ] ++ right


{-| Apply a transformation function to every element in the ZipperRing.
-}
map : (a -> b) -> ZipperRing a -> ZipperRing b
map mutator (ZipperRing left focused right) =
    ZipperRing (List.map mutator left) (mutator focused) (List.map mutator right)


{-| Apply a function to the element the ZipperRing is currently focused on.
-}
mapFocused : (a -> a) -> ZipperRing a -> ZipperRing a
mapFocused mutator (ZipperRing left focused right) =
    ZipperRing left (mutator focused) right
