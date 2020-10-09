module ColorPicker exposing (..)

{-| Color picker example backed by a FocusRing.

This example uses elm-css to demonstrate the usage and compatibility with another UI library.

-}

import Browser
import Browser.Events exposing (onKeyDown)
import Css exposing (..)
import Css.Colors exposing (..)
import FocusRing exposing (FocusRing)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
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
        [ darkred
        , crimson
        , tomato
        , plum
        , mediumpurple
        , rebeccapurple
        , darkblue
        , royalblue
        , cornflowerblue
        , yellowgreen
        , forestgreen
        , darkgreen
        , chocolate
        , orange
        , gold
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
subscriptions _ =
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
        [ h1
            [ css [ padding (px 12) ] ]
            [ text "Pick your color" ]
        , div []
            [ colorPickerView model.colors ]
        , div
            [ css
                [ property "clear" "both"
                , padding2 (px 20) (px 12)
                ]
            ]
            [ focusedColorDetail model.colors ]
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
            [ css
                [ float left
                , backgroundColor color
                , cursor pointer
                , padding2 (px 20) (px 30)
                ]
            ]
            []
        ]


{-| Show a tile representation of the provided focused color.
-}
focusedColorTile : Color -> Html Msg
focusedColorTile color =
    a [ onClick (ColorClicked color) ]
        [ div
            [ css
                [ float left
                , backgroundColor color
                , border3 (px 1) solid (hex "696969")
                , boxShadow5 (px 4) (px 2) (px 10) (px 1) (hex "2f2f2f")
                , cursor pointer
                , padding2 (px 24) (px 32)
                , marginTop (px -5)
                ]
            ]
            []
        ]


{-| Show a left chevron for selecting previous color.
-}
chevronLeft : Html Msg
chevronLeft =
    div
        [ css
            [ float left
            , fontSize (px 30)
            , cursor pointer
            , property "user-select" "none"
            , margin2 zero (px 8)
            ]
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
        [ css
            [ float left
            , fontSize (px 30)
            , cursor pointer
            , property "user-select" "none"
            , margin2 zero (px 8)
            ]
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
        color =
            FocusRing.getFocused colors
                |> Maybe.withDefault white
    in
    div []
        [ div
            [ css
                [ float left
                , padding2 (px 10) zero
                ]
            ]
            [ strong [] [ text "Focused: " ]
            , span [] [ text color.value ]
            ]
        , div
            [ css
                [ property "clear" "both"
                , float left
                , backgroundColor color
                , border3 (px 1) solid (hex "696969")
                , padding2 (px 60) (px 100)
                ]
            ]
            []
        ]
