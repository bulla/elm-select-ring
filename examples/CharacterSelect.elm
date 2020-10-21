module CharacterSelect exposing (..)

{-| In-game character selector example backed by a FocusRing.

Credits to Neslug for the Mario GIFs <https://www.deviantart.com/neslug/gallery/3553116/super-mario>

-}

import Browser
import Browser.Events exposing (onKeyDown)
import FocusRing exposing (FocusRing)
import Html exposing (Html, a, div, h1, img, strong, text)
import Html.Attributes exposing (alt, height, src, style, title)
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
    { characters : FocusRing Character
    }


type Character
    = Mario
    | Peach
    | Wario
    | Yoshi


init : () -> ( Model, Cmd Msg )
init _ =
    ( { characters = initCharacters
      }
    , Cmd.none
    )


initCharacters : FocusRing Character
initCharacters =
    FocusRing.fromList [ Mario, Peach, Wario, Yoshi ]



-- UPDATE


type Msg
    = PreviousClicked
    | NextClicked
    | CharacterClicked Character
    | ButtonPressed Button


type Button
    = LeftArrow
    | RightArrow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PreviousClicked ->
            ( focusOnPreviousCharacter model
            , Cmd.none
            )

        NextClicked ->
            ( focusOnNextCharacter model
            , Cmd.none
            )

        CharacterClicked character ->
            ( focusOnCharacter character model
            , Cmd.none
            )

        ButtonPressed button ->
            ( applyButtonPress button model
            , Cmd.none
            )


{-| Focus on the previous character.
-}
focusOnPreviousCharacter : Model -> Model
focusOnPreviousCharacter model =
    { model
        | characters = FocusRing.focusOnPrevious model.characters
    }


{-| Focus on the next character.
-}
focusOnNextCharacter : Model -> Model
focusOnNextCharacter model =
    { model
        | characters = FocusRing.focusOnNext model.characters
    }


{-| Toggle selection of the provided item.
-}
focusOnCharacter : Character -> Model -> Model
focusOnCharacter character model =
    { model
        | characters = FocusRing.focusOnFirstMatching (\c -> c == character) model.characters
    }


{-| Apply the corresponding action in response of a key pressed.
-}
applyButtonPress : Button -> Model -> Model
applyButtonPress button model =
    case button of
        LeftArrow ->
            focusOnPreviousCharacter model

        RightArrow ->
            focusOnNextCharacter model



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
        [ h1 [ style "padding" "12px" ]
            [ text "Choose your character" ]
        , div []
            [ characterSelectorView model.characters ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ focusedCharacterView model.characters ]
        ]


{-| Show a representation of the characters FocusRing.
-}
characterSelectorView : FocusRing Character -> Html Msg
characterSelectorView characters =
    let
        characterImages =
            characters
                |> FocusRing.mapEachIntoList characterImage focusedCharacterImage
    in
    div []
        [ chevronLeft
        , div [] characterImages
        , chevronRight
        ]


{-| Show a still representation of the provided character.
-}
characterImage : Character -> Html Msg
characterImage character =
    let
        characterName =
            characterToString character
    in
    div
        [ style "float" "left"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ a [ onClick (CharacterClicked character) ]
            [ img
                [ src ("images/" ++ String.toLower characterName ++ ".gif")
                , alt characterName
                , title characterName
                , height 64
                ]
                []
            ]
        ]


{-| Show an animated representation of the focused character that has been provided.
-}
focusedCharacterImage : Character -> Html Msg
focusedCharacterImage character =
    let
        characterName =
            characterToString character
    in
    div
        [ style "float" "left"
        , style "background-color" "#dedede"
        , style "border" "#b189f5 4px dashed"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "4px"
        ]
        [ a [ onClick (CharacterClicked character) ]
            [ img
                [ src ("images/" ++ String.toLower characterName ++ "-animated.gif")
                , alt characterName
                , title characterName
                , height 64
                ]
                []
            ]
        ]


{-| Convert the provided character to its String representation.
-}
characterToString : Character -> String
characterToString character =
    case character of
        Mario ->
            "Mario"

        Peach ->
            "Peach"

        Wario ->
            "Wario"

        Yoshi ->
            "Yoshi"


{-| Show a left chevron for selecting previous character.
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


{-| Show a right chevron for selecting next character.
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


{-| Show the name of the currently focused character.
-}
focusedCharacterView : FocusRing Character -> Html msg
focusedCharacterView characters =
    let
        characterName =
            FocusRing.getFocused characters
                |> Maybe.map characterToString
                |> Maybe.withDefault "None"
    in
    div [ style "margin-top" "4px" ]
        [ strong []
            [ text "Focused: " ]
        , text characterName
        ]
