module ItemSelect exposing (..)

{-| In-game item selector example backed by a SelectRing.

Credits to Neslug for the Mario GIFs <https://www.deviantart.com/neslug/gallery/3553116/super-mario>

-}

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, a, div, h1, img, strong, text)
import Html.Attributes exposing (alt, height, src, style, title)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import SelectRing exposing (SelectRing)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { items : SelectRing Item
    }


type Item
    = Banana
    | Lightning
    | Mushroom
    | Shell
    | Star


init : () -> ( Model, Cmd Msg )
init _ =
    ( { items = initItems
      }
    , Cmd.none
    )


initItems : SelectRing Item
initItems =
    SelectRing.fromList [ Banana, Lightning, Mushroom, Shell, Star ]



-- UPDATE


type Msg
    = PreviousClicked
    | NextClicked
    | ItemClicked Item
    | ButtonPressed Button


type Button
    = LeftArrow
    | RightArrow
    | Space
    | Enter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PreviousClicked ->
            ( focusOnPreviousItem model
            , Cmd.none
            )

        NextClicked ->
            ( focusOnNextItem model
            , Cmd.none
            )

        ItemClicked item ->
            ( toggleItem item model
            , Cmd.none
            )

        ButtonPressed button ->
            ( applyButtonPress button model
            , Cmd.none
            )


{-| Focus on the previous item.
-}
focusOnPreviousItem : Model -> Model
focusOnPreviousItem model =
    { model
        | items = SelectRing.focusOnPrevious model.items
    }


{-| Focus on the next item.
-}
focusOnNextItem : Model -> Model
focusOnNextItem model =
    { model
        | items = SelectRing.focusOnNext model.items
    }


{-| Toggle selection of the provided item.
-}
toggleItem : Item -> Model -> Model
toggleItem toggledItem model =
    { model
        | items =
            model.items
                |> SelectRing.focusOnFirstMatching (\item -> item == toggledItem)
                |> SelectRing.toggleFocused
    }


{-| Toggle selection of the currently focused item.
-}
toggleFocused : Model -> Model
toggleFocused model =
    { model
        | items = SelectRing.toggleFocused model.items
    }


{-| Apply the corresponding action in response of a key pressed.
-}
applyButtonPress : Button -> Model -> Model
applyButtonPress button model =
    case button of
        LeftArrow ->
            focusOnPreviousItem model

        RightArrow ->
            focusOnNextItem model

        Space ->
            toggleFocused model

        Enter ->
            toggleFocused model



-- SUBSCRIPTIONS


{-| Subscribe to the key pressed events.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map ButtonPressed decodeButton)
        ]


{-| Decode a key related event into its corresponding Button event.
-}
decodeButton : Decoder Button
decodeButton =
    Decode.field "key" Decode.string
        |> Decode.andThen toButton


{-| Convert a key name into its corresponding Button.
-}
toButton : String -> Decoder Button
toButton key =
    case key of
        "ArrowLeft" ->
            Decode.succeed LeftArrow

        "ArrowRight" ->
            Decode.succeed RightArrow

        " " ->
            Decode.succeed Space

        "Enter" ->
            Decode.succeed Enter

        _ ->
            Decode.fail "Skip"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style "padding" "12px" ]
            [ text "Pick an item" ]
        , div []
            [ itemSelectorView model.items ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ focusedItemView model.items
            , selectedItemView model.items
            ]
        ]


{-| Show a representation of the items SelectRing.
-}
itemSelectorView : SelectRing Item -> Html Msg
itemSelectorView items =
    let
        itemImages =
            items
                |> SelectRing.mapEachIntoList itemImage (focusedItemImage items) selectedItemImage
    in
    div []
        [ chevronLeft
        , div [] itemImages
        , chevronRight
        ]


{-| Show a still representation of the provided item.
-}
itemImage : Item -> Html Msg
itemImage item =
    let
        itemName =
            itemToString item
    in
    div
        [ style "float" "left"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ a [ onClick (ItemClicked item) ]
            [ img
                [ src ("images/" ++ String.toLower itemName ++ ".gif")
                , alt itemName
                , title itemName
                , height 64
                ]
                []
            ]
        ]


{-| Show a representation of the focused item that has been provided.
-}
focusedItemImage : SelectRing Item -> Item -> Html Msg
focusedItemImage items focusedItem =
    let
        itemName =
            itemToString focusedItem

        isItemSelected =
            items
                |> SelectRing.isSelectedMatching (\item -> item == focusedItem)

        backgroundColor =
            if isItemSelected then
                "#dedede"

            else
                "white"
    in
    div
        [ style "float" "left"
        , style "background-color" backgroundColor
        , style "border" "#b189f5 4px dashed"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "4px"
        ]
        [ a [ onClick (ItemClicked focusedItem) ]
            [ img
                [ src ("images/" ++ String.toLower itemName ++ "-animated.gif")
                , alt itemName
                , title itemName
                , height 64
                ]
                []
            ]
        ]


{-| Show a representation of the selected item that has been provided.
-}
selectedItemImage : Item -> Html Msg
selectedItemImage selectedItem =
    let
        itemName =
            itemToString selectedItem
    in
    div
        [ style "float" "left"
        , style "background-color" "#dedede"
        , style "border" "#a1a1a1 2px dashed"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "4px"
        ]
        [ a [ onClick (ItemClicked selectedItem) ]
            [ img
                [ src ("images/" ++ String.toLower itemName ++ "-animated.gif")
                , alt itemName
                , title itemName
                , height 64
                ]
                []
            ]
        ]


{-| Convert the provided item to its String representation.
-}
itemToString : Item -> String
itemToString item =
    case item of
        Banana ->
            "Banana"

        Lightning ->
            "Lightning"

        Mushroom ->
            "Mushroom"

        Shell ->
            "Shell"

        Star ->
            "Star"


{-| Show a left chevron for selecting previous item.
-}
chevronLeft : Html Msg
chevronLeft =
    div
        [ style "float" "left"
        , style "font-size" "32px"
        , style "cursor" "pointer"
        , style "user-select" "none"
        , style "margin" "24px 12px"
        ]
        [ a
            [ title "Previous"
            , onClick PreviousClicked
            ]
            [ text "❮" ]
        ]


{-| Show a right chevron for selecting next item.
-}
chevronRight : Html Msg
chevronRight =
    div
        [ style "float" "left"
        , style "font-size" "32px"
        , style "cursor" "pointer"
        , style "user-select" "none"
        , style "margin" "24px 12px"
        ]
        [ a
            [ title "Next"
            , onClick NextClicked
            ]
            [ text "❱" ]
        ]


{-| Show the name of the currently focused item.
-}
focusedItemView : SelectRing Item -> Html msg
focusedItemView items =
    let
        itemName =
            items
                |> SelectRing.getFocused
                |> Maybe.map itemToString
                |> Maybe.withDefault "None"
    in
    div [ style "margin-top" "4px" ]
        [ strong []
            [ text "Focused: " ]
        , text itemName
        ]


{-| Show the name of the currently selected item.
-}
selectedItemView : SelectRing Item -> Html msg
selectedItemView items =
    let
        itemName =
            items
                |> SelectRing.getSelected
                |> Maybe.map itemToString
                |> Maybe.withDefault "None"
    in
    div [ style "margin-top" "4px" ]
        [ strong []
            [ text "Selected: " ]
        , text itemName
        ]
