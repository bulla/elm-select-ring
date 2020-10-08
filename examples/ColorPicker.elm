module ColorPicker exposing (..)

{-| Color picker example backed by a FocusRing.
-}

import Browser
import Browser.Events exposing (onKeyDown)
import Color exposing (Color)
import FocusRing exposing (FocusRing)
import Html exposing (Html, a, div, h1, strong, text)
import Html.Attributes exposing (style, title)
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
    { colors : FocusRing Color }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { colors = initColors }
    , Cmd.none
    )


initColors : FocusRing Color
initColors =
    FocusRing.fromList
        [ Color.darkRed
        , Color.red
        , Color.lightRed
        , Color.lightPurple
        , Color.purple
        , Color.darkPurple
        , Color.darkBlue
        , Color.blue
        , Color.lightBlue
        , Color.lightGreen
        , Color.green
        , Color.darkGreen
        , Color.darkYellow
        , Color.yellow
        , Color.lightYellow
        , Color.lightOrange
        , Color.orange
        , Color.darkOrange
        ]



-- UPDATE


type Msg
    = PreviousClicked
    | NextClicked
    | ColorClicked Color
    | ButtonPressed Button


type Button
    = LeftArrow
    | RightArrow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PreviousClicked ->
            ( focusOnPreviousColor model
            , Cmd.none
            )

        NextClicked ->
            ( focusOnNextColor model
            , Cmd.none
            )

        ColorClicked color ->
            ( focusOnColor color model
            , Cmd.none
            )

        ButtonPressed button ->
            ( applyButtonPress button model
            , Cmd.none
            )


{-| Focus on the previous color.
-}
focusOnPreviousColor : Model -> Model
focusOnPreviousColor model =
    { model
        | colors = FocusRing.focusOnPrevious model.colors
    }


{-| Focus on the next color.
-}
focusOnNextColor : Model -> Model
focusOnNextColor model =
    { model
        | colors = FocusRing.focusOnNext model.colors
    }


{-| Toggle selection of the provided item.
-}
focusOnColor : Color -> Model -> Model
focusOnColor color model =
    { model
        | colors = FocusRing.focusOnFirstMatching (\c -> c == color) model.colors
    }


{-| Apply the corresponding action in response of a key pressed.
-}
applyButtonPress : Button -> Model -> Model
applyButtonPress button model =
    case button of
        LeftArrow ->
            focusOnPreviousColor model

        RightArrow ->
            focusOnNextColor model



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

        _ ->
            Decode.fail "Skip"



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style "padding" "12px" ]
            [ text "Pick your color" ]
        , div []
            [ colorPickerView model.colors ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ focusedCssColor model.colors
            , focusedColorDetail model.colors
            ]
        ]


{-| Show a color picker representation with all colors composing the FocusRing.
-}
colorPickerView : FocusRing Color -> Html Msg
colorPickerView colors =
    let
        colorTiles =
            colors
                |> FocusRing.mapEachIntoList colorTile focusedColorTile
    in
    div []
        [ chevronLeft
        , div [] colorTiles
        , chevronRight
        ]


{-| Show a tile representation of the provided color.
-}
colorTile : Color -> Html Msg
colorTile color =
    a [ onClick (ColorClicked color) ]
        [ div
            [ style "float" "left"
            , style "background-color" (Color.toCssString color)
            , style "cursor" "pointer"
            , style "padding" "20px 30px"
            ]
            []
        ]


{-| Show a tile representation of the provided focused color.
-}
focusedColorTile : Color -> Html Msg
focusedColorTile color =
    a [ onClick (ColorClicked color) ]
        [ div
            [ style "float" "left"
            , style "background-color" (Color.toCssString color)
            , style "border" "1px solid #696969"
            , style "box-shadow" "4px 2px 10px 1px #2f2f2f"
            , style "cursor" "pointer"
            , style "padding" "24px 32px"
            , style "margin-top" "-5px"
            ]
            []
        ]


{-| Show a left chevron for selecting previous color.
-}
chevronLeft : Html Msg
chevronLeft =
    div
        [ style "float" "left"
        , style "font-size" "30px"
        , style "cursor" "pointer"
        , style "user-select" "none"
        , style "margin" "0 8px"
        ]
        [ a
            [ title "Previous"
            , onClick PreviousClicked
            ]
            [ text "❮" ]
        ]


{-| Show a right chevron for selecting next color.
-}
chevronRight : Html Msg
chevronRight =
    div
        [ style "float" "left"
        , style "font-size" "30px"
        , style "cursor" "pointer"
        , style "user-select" "none"
        , style "margin" "0 8px"
        ]
        [ a
            [ title "Next"
            , onClick NextClicked
            ]
            [ text "❱" ]
        ]


{-| Show a representation of the currently focused color as a bigger tile.
-}
focusedColorDetail : FocusRing Color -> Html msg
focusedColorDetail colors =
    let
        cssColor =
            FocusRing.getFocused colors
                |> Maybe.map Color.toCssString
                |> Maybe.withDefault "white"
    in
    div
        [ style "float" "left"
        , style "background-color" cssColor
        , style "border" "1px solid #696969"
        , style "padding" "60px 100px"
        ]
        []


{-| Show a representation of the currently focused color as a String.
-}
focusedCssColor : FocusRing Color -> Html msg
focusedCssColor colors =
    let
        cssColor =
            FocusRing.getFocused colors
                |> Maybe.map Color.toCssString
                |> Maybe.withDefault "None"
    in
    div
        [ style "clear" "both"
        , style "padding" "10px 0"
        ]
        [ strong []
            [ text "Focused: " ]
        , text cssColor
        ]
