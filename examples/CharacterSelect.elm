module CharacterSelect exposing (..)

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


previousCharacter : Model -> Model
previousCharacter model =
    { model
        | characters = FocusRing.focusOnPrevious model.characters
    }


nextCharacter : Model -> Model
nextCharacter model =
    { model
        | characters = FocusRing.focusOnNext model.characters
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Select your character" ]
        , div []
            [ characterSelectorView model.characters ]
        , div [ style "clear" "both" ]
            [ selectedCharacterView model.characters ]
        ]


characterSelectorView : FocusRing Character -> Html Msg
characterSelectorView characters =
    let
        characterImages =
            characters
                |> FocusRing.mapEachIntoList stillImage animatedImage
    in
    div []
        [ chevronLeft
        , div [] characterImages
        , chevronRight
        ]


stillImage : Character -> Html msg
stillImage character =
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


animatedImage : Character -> Html msg
animatedImage character =
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
            [ src ("images/" ++ String.toLower characterName ++ "-sprite.gif")
            , alt characterName
            , title characterName
            , height 64
            ]
            []
        ]


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


chevronLeft : Html Msg
chevronLeft =
    div
        [ style "float" "left"
        , style "font-size" "32px"
        , style "cursor" "pointer"
        , style "margin" "18px"
        ]
        [ a [ onClick PreviousClicked ]
            [ text "<" ]
        ]


chevronRight : Html Msg
chevronRight =
    div
        [ style "float" "left"
        , style "font-size" "32px"
        , style "cursor" "pointer"
        , style "margin" "18px"
        ]
        [ a [ onClick NextClicked ]
            [ text ">" ]
        ]


selectedCharacterView : FocusRing Character -> Html msg
selectedCharacterView characters =
    let
        characterName =
            FocusRing.getFocused characters
                |> Maybe.map characterToString
                |> Maybe.withDefault "None"
    in
    div [ style "padding" "20px 10px" ]
        [ text ("Selected: " ++ characterName)
        ]
