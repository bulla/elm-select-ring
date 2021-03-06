module DiceSelect exposing (..)

{-| Random dice selector example backed by a MultiSelectRing.

Credits to Gearstd for the 3D rendered black dice images:
<https://www.shutterstock.com/image-illustration/3d-rendering-set-six-black-dice-751136791>

-}

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder)
import MultiSelectRing exposing (MultiSelectRing)
import Random



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
    { dice : MultiSelectRing Die
    , score : Int
    }


{-| Die face definition for the D6.
-}
type DieFace
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


{-| Rolled die definition that includes its position (or id) for comparison purposes.
-}
type Die
    = Die Int DieFace


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dice = MultiSelectRing.empty
      , score = 0
      }
    , Random.generate DiceRolled (randomDiceGenerator 5)
    )


{-| Random generator to generate a list of rolled dice of the provided length.
-}
randomDiceGenerator : Int -> Random.Generator (List DieFace)
randomDiceGenerator count =
    Random.int 1 6
        |> Random.list count
        |> Random.map (\dice -> List.map intToDieFace dice)



-- UPDATE


type Msg
    = RollDice
    | DiceRolled (List DieFace)
    | DieClicked Die
    | ButtonPressed Button


type Button
    = LeftArrow
    | RightArrow
    | Space
    | Enter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model
            , Random.generate DiceRolled (randomDiceGenerator 5)
            )

        DiceRolled dieFaces ->
            ( assignRolledDice dieFaces model
            , Cmd.none
            )

        DieClicked die ->
            ( toggleDie die model
            , Cmd.none
            )

        ButtonPressed button ->
            ( applyButtonPress button model
            , Cmd.none
            )


assignRolledDice : List DieFace -> Model -> Model
assignRolledDice dieFaces model =
    let
        dice =
            dieFaces
                |> List.indexedMap (\position face -> Die position face)
    in
    { model
        | dice = MultiSelectRing.fromList dice
        , score = 0
    }


{-| Focus on the previous die.
-}
focusOnPreviousItem : Model -> Model
focusOnPreviousItem model =
    { model
        | dice = MultiSelectRing.focusOnPrevious model.dice
    }


{-| Focus on the next die.
-}
focusOnNextItem : Model -> Model
focusOnNextItem model =
    { model
        | dice = MultiSelectRing.focusOnNext model.dice
    }


{-| Toggle selection of the provided die.
-}
toggleDie : Die -> Model -> Model
toggleDie toggledDie model =
    let
        dice =
            model.dice
                |> MultiSelectRing.focusOnFirstMatching (\die -> die == toggledDie)
                |> MultiSelectRing.toggleFocused
    in
    { model
        | dice = dice
        , score = sumSelectedDice dice
    }


{-| Toggle selection of the currently focused die.
-}
toggleFocused : Model -> Model
toggleFocused model =
    let
        dice =
            MultiSelectRing.toggleFocused model.dice
    in
    { model
        | dice = dice
        , score = sumSelectedDice dice
    }


{-| Return the sum of the selected dice values.
-}
sumSelectedDice : MultiSelectRing Die -> Int
sumSelectedDice dice =
    MultiSelectRing.getSelected dice
        |> List.map dieToInt
        |> List.sum


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
            [ text "Pick a few dice" ]
        , div [ style "padding" "8px 12px" ]
            [ text ("Score: " ++ String.fromInt model.score) ]
        , div [ style "padding" "0 12px" ]
            [ diceSelectorView model.dice ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ button [ onClick RollDice ] [ text "Roll again" ] ]
        ]


{-| Show a representation of the dice MultiSelectRing.
-}
diceSelectorView : MultiSelectRing Die -> Html Msg
diceSelectorView dice =
    let
        diceImages =
            dice
                |> MultiSelectRing.mapEachIntoList
                    dieFaceImage
                    (focusedDieFaceImage dice)
                    selectedDieFaceImage
    in
    div [] diceImages


{-| Show a representation of the provided die.
-}
dieFaceImage : Die -> Html Msg
dieFaceImage die =
    let
        dieValue =
            dieToString die
    in
    div
        [ style "float" "left"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ a [ onClick (DieClicked die) ]
            [ img
                [ src ("../images/die-face-" ++ dieValue ++ ".png")
                , alt dieValue
                , title dieValue
                , width 80
                ]
                []
            ]
        ]


{-| Show a representation of the focused die that has been provided.
-}
focusedDieFaceImage : MultiSelectRing Die -> Die -> Html Msg
focusedDieFaceImage dice focusedDie =
    let
        dieValue =
            dieToString focusedDie

        backgroundColor =
            if MultiSelectRing.isFocusedSelected dice then
                "#dedede"

            else
                "white"
    in
    div
        [ style "float" "left"
        , style "background-color" backgroundColor
        , style "border" "#b189f5 4px dashed"
        , style "border-radius" "20px"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ a [ onClick (DieClicked focusedDie) ]
            [ img
                [ src ("../images/die-face-" ++ dieValue ++ ".png")
                , alt dieValue
                , title dieValue
                , width 80
                ]
                []
            ]
        ]


{-| Show a representation of the selected die that has been provided.
-}
selectedDieFaceImage : Die -> Html Msg
selectedDieFaceImage selectedDie =
    let
        dieValue =
            dieToString selectedDie
    in
    div
        [ style "float" "left"
        , style "background-color" "#dedede"
        , style "border" "#a1a1a1 2px dashed"
        , style "border-radius" "20px"
        , style "cursor" "pointer"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ a [ onClick (DieClicked selectedDie) ]
            [ img
                [ src ("../images/die-face-" ++ dieValue ++ ".png")
                , alt dieValue
                , title dieValue
                , width 80
                ]
                []
            ]
        ]


{-| Convert the provided integer value of a die to its corresponding DieFace.
-}
intToDieFace : Int -> DieFace
intToDieFace value =
    case value of
        1 ->
            One

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        _ ->
            One


{-| Convert the provided die to its corresponding integer value.
-}
dieToInt : Die -> Int
dieToInt (Die _ face) =
    case face of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


{-| Convert the provided die to a string representation of its integer value.
-}
dieToString : Die -> String
dieToString die =
    String.fromInt (dieToInt die)
