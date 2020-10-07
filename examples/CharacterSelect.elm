module CharacterSelect exposing (..)

{-| In-game character selector example backed by a FocusRing.

Credits to Neslug for the Mario GIFs <https://www.deviantart.com/neslug/gallery/3553116/super-mario>

-}

import Browser
import FocusRing exposing (FocusRing)
import Html exposing (Html, a, div, h1, img, text)
import Html.Attributes exposing (alt, height, src, style, title)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PreviousClicked ->
            ( previousCharacter model
            , Cmd.none
            )

        NextClicked ->
            ( nextCharacter model
            , Cmd.none
            )


{-| Focus on the previous character
-}
previousCharacter : Model -> Model
previousCharacter model =
    { model
        | characters = FocusRing.focusOnPrevious model.characters
    }


{-| Focus on the next character
-}
nextCharacter : Model -> Model
nextCharacter model =
    { model
        | characters = FocusRing.focusOnNext model.characters
    }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style "padding" "12px" ]
            [ text "Select your character" ]
        , div []
            [ characterSelectorView model.characters ]
        , div [ style "clear" "both" ]
            [ selectedCharacterView model.characters ]
        ]


{-| Show a representation of the characters FocusRing.
-}
characterSelectorView : FocusRing Character -> Html Msg
characterSelectorView characters =
    let
        characterImages =
            characters
                |> FocusRing.mapEachIntoList stillCharacterImage animatedCharacterImage
    in
    div []
        [ chevronLeft
        , div [] characterImages
        , chevronRight
        ]


{-| Show a still representation of the provided character.
-}
stillCharacterImage : Character -> Html msg
stillCharacterImage character =
    let
        characterName =
            characterToString character
    in
    div
        [ style "float" "left"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ img
            [ src ("images/" ++ String.toLower characterName ++ ".gif")
            , alt characterName
            , title characterName
            , height 64
            ]
            []
        ]


{-| Show an animated representation of the provided character.
-}
animatedCharacterImage : Character -> Html msg
animatedCharacterImage character =
    let
        characterName =
            characterToString character
    in
    div
        [ style "float" "left"
        , style "border" "#b189f5 4px dashed"
        , style "margin" "8px"
        , style "padding" "4px"
        ]
        [ img
            [ src ("images/" ++ String.toLower characterName ++ "-animated.gif")
            , alt characterName
            , title characterName
            , height 64
            ]
            []
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


{-| Show the name of the currently selected character.
-}
selectedCharacterView : FocusRing Character -> Html msg
selectedCharacterView characters =
    let
        characterName =
            FocusRing.getFocused characters
                |> Maybe.map characterToString
                |> Maybe.withDefault "None"
    in
    div [ style "padding" "20px 12px" ]
        [ text ("Selected: " ++ characterName)
        ]
