module MenuSelect exposing (..)

{-| In-game menu selector example backed by a FocusRing.
-}

import Browser
import Browser.Events exposing (onKeyDown)
import FocusRing exposing (FocusRing)
import Html exposing (Html, a, div, h1, strong, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)



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
    { menu : FocusRing Option }


type Option
    = NewGame
    | LoadGame
    | Settings
    | Exit


init : () -> ( Model, Cmd Msg )
init _ =
    ( { menu = initMenu }
    , Cmd.none
    )


initMenu : FocusRing Option
initMenu =
    FocusRing.fromList
        [ NewGame
        , LoadGame
        , Settings
        , Exit
        ]



-- UPDATE


type Msg
    = OptionClicked Option
    | ButtonPressed Button


type Button
    = UpArrow
    | DownArrow
    | Space
    | Enter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OptionClicked option ->
            ( focusOnOption option model
            , Cmd.none
            )

        ButtonPressed button ->
            ( applyButtonPress button model
            , Cmd.none
            )


{-| Focus on the previous option.
-}
focusOnPreviousOption : Model -> Model
focusOnPreviousOption model =
    { model
        | menu = FocusRing.focusOnPrevious model.menu
    }


{-| Focus on the next option.
-}
focusOnNextOption : Model -> Model
focusOnNextOption model =
    { model
        | menu = FocusRing.focusOnNext model.menu
    }


{-| Toggle selection of the provided option.
-}
focusOnOption : Option -> Model -> Model
focusOnOption option model =
    { model
        | menu = FocusRing.focusOnFirstMatching (\c -> c == option) model.menu
    }


confirmFocusedOption : Model -> Model
confirmFocusedOption model =
    model


{-| Apply the corresponding action in response of a key pressed.
-}
applyButtonPress : Button -> Model -> Model
applyButtonPress button model =
    case button of
        UpArrow ->
            focusOnPreviousOption model

        DownArrow ->
            focusOnNextOption model

        Space ->
            confirmFocusedOption model

        Enter ->
            confirmFocusedOption model



-- SUBSCRIPTIONS


{-| Subscribe to the key pressed events.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map ButtonPressed decodeButton) ]


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
        "ArrowUp" ->
            Decode.succeed UpArrow

        "ArrowDown" ->
            Decode.succeed DownArrow

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
            [ text "Menu" ]
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "40px auto"
            , style "grid-template-rows" "repeat(4 60px)"
            , style "column-gap" "8px"
            , style "row-gap" "8px"
            ]
            (menuSelectorView model.menu)
        , div
            [ style "padding" "20px 12px" ]
            [ focusedOptionView model.menu ]
        ]


{-| Show a representation of the options FocusRing.
-}
menuSelectorView : FocusRing Option -> List (Html Msg)
menuSelectorView menu =
    let
        menuOptions =
            menu
                |> FocusRing.mapEachIntoList optionText focusedOptionText
    in
    pointer menu :: menuOptions


{-| Show a still representation of the provided option.
-}
optionText : Option -> Html Msg
optionText option =
    div
        [ style "cursor" "pointer"
        , style "grid-column-start" "2"
        , style "justify-self" "left"
        , style "padding" "2px"
        ]
        [ a [ onClick (OptionClicked option) ]
            [ text (optionToString option) ]
        ]


{-| Show an animated representation of the focused option that has been provided.
-}
focusedOptionText : Option -> Html Msg
focusedOptionText option =
    div
        [ style "background-color" "#dedede"
        , style "border" "#b189f5 4px dashed"
        , style "cursor" "pointer"
        , style "grid-column-start" "2"
        , style "justify-self" "left"
        , style "padding" "4px"
        ]
        [ a [ onClick (OptionClicked option) ]
            [ text (optionToString option) ]
        ]


{-| Convert the provided option to its String representation.
-}
optionToString : Option -> String
optionToString option =
    case option of
        NewGame ->
            "New Game"

        LoadGame ->
            "Load Game"

        Settings ->
            "Settings"

        Exit ->
            "Exit"


{-| Show a right chevron for selecting next option.
-}
pointer : FocusRing Option -> Html Msg
pointer menu =
    let
        row =
            FocusRing.getFocusedIndex menu + 1
    in
    div
        [ style "font-size" "24px"
        , style "grid-column" "1"
        , style "grid-row" (String.fromInt row)
        , style "user-select" "none"
        , style "justify-self" "center"
        ]
        [ text "❱" ]


{-| Show the name of the currently focused option.
-}
focusedOptionView : FocusRing Option -> Html msg
focusedOptionView menu =
    let
        optionName =
            FocusRing.getFocused menu
                |> Maybe.map optionToString
                |> Maybe.withDefault "None"
    in
    div [ style "margin-top" "4px" ]
        [ strong []
            [ text "Focused: " ]
        , text optionName
        ]
