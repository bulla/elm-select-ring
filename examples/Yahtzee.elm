module Yahtzee exposing (..)

import Array
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
    , turn : GameTurn
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


type GameTurn
    = Initializing
    | FirstTurn
    | SecondTurn
    | LastTurn
    | GameOver


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dice = initDice
      , score = 0
      , turn = Initializing
      }
    , Random.generate DiceRolled fiveRandomDice
    )


initDice : MultiSelectRing Die
initDice =
    MultiSelectRing.selectAll <|
        MultiSelectRing.fromList
            [ Die 0 One
            , Die 1 One
            , Die 2 One
            , Die 3 One
            , Die 4 One
            ]


{-| Random generator to generate a list of rolled dice of the provided length.
-}
randomDiceGenerator : Int -> Random.Generator (List DieFace)
randomDiceGenerator count =
    Random.int 1 6
        |> Random.list count
        |> Random.map (\dice -> List.map intToDieFace dice)


fiveRandomDice : Random.Generator (List DieFace)
fiveRandomDice =
    randomDiceGenerator 5



-- UPDATE


type Msg
    = RollSelectedDice
    | DiceRolled (List DieFace)
    | DieClicked Die
    | ButtonPressed Button
    | AllDiceScored
    | GameRestarted


type Button
    = LeftArrow
    | RightArrow
    | Space
    | Enter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollSelectedDice ->
            ( model
            , Random.generate DiceRolled fiveRandomDice
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

        AllDiceScored ->
            ( finalizeScore model
            , Cmd.none
            )

        GameRestarted ->
            init ()


assignRolledDice : List DieFace -> Model -> Model
assignRolledDice dieFaces model =
    let
        rolledDice =
            dieFaces
                |> List.indexedMap (\position face -> Die position face)
                |> Array.fromList

        selectedDieToRolledDie : Die -> Die
        selectedDieToRolledDie ((Die index _) as die) =
            if MultiSelectRing.isSelectedAt index model.dice then
                Array.get index rolledDice
                    |> Maybe.withDefault die

            else
                die

        dice =
            model.dice
                |> MultiSelectRing.map selectedDieToRolledDie
                |> MultiSelectRing.deselectAll
                |> MultiSelectRing.focusOnFirst
    in
    { model
        | dice = dice
        , score = sumDice dice
        , turn = nextTurn model.turn
    }


nextTurn : GameTurn -> GameTurn
nextTurn turn =
    case turn of
        Initializing ->
            FirstTurn

        FirstTurn ->
            SecondTurn

        SecondTurn ->
            LastTurn

        LastTurn ->
            GameOver

        GameOver ->
            Initializing


{-| Focus on the previous item.
-}
focusOnPreviousItem : Model -> Model
focusOnPreviousItem model =
    { model
        | dice = MultiSelectRing.focusOnPrevious model.dice
    }


{-| Focus on the next item.
-}
focusOnNextItem : Model -> Model
focusOnNextItem model =
    { model
        | dice = MultiSelectRing.focusOnNext model.dice
    }


{-| Toggle selection of the provided die.
-}
toggleDie : Die -> Model -> Model
toggleDie (Die index _) model =
    { model
        | dice =
            model.dice
                |> MultiSelectRing.focusOn index
                |> MultiSelectRing.toggleFocused
    }


{-| Toggle selection of the currently focused die.
-}
toggleFocused : Model -> Model
toggleFocused model =
    { model
        | dice = MultiSelectRing.toggleFocused model.dice
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


finalizeScore : Model -> Model
finalizeScore model =
    { model
        | turn = GameOver
    }


sumDice : MultiSelectRing Die -> Int
sumDice dice =
    MultiSelectRing.toList dice
        |> List.map dieToInt
        |> List.sum



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
            [ text "Yahtzee!" ]
        , div [ style "padding" "8px 12px" ]
            [ text ("Turn: " ++ Debug.toString model.turn) ]
        , div [ style "padding" "8px 12px" ]
            [ text ("Score: " ++ String.fromInt model.score) ]
        , div [ style "padding" "0 12px" ]
            [ diceSelectorView model.dice model.turn ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ gameActions model.dice model.turn ]
        ]


{-| Show a representation of the dice MultiSelectRing.
-}
diceSelectorView : MultiSelectRing Die -> GameTurn -> Html Msg
diceSelectorView dice turn =
    let
        diceImages =
            if turn == GameOver then
                dice
                    |> MultiSelectRing.mapEachIntoList
                        dieFaceImage
                        dieFaceImage
                        dieFaceImage

            else
                dice
                    |> MultiSelectRing.mapEachIntoList
                        clickableDieFaceImage
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
        , style "user-select" "none"
        , style "margin" "8px"
        , style "padding" "2px"
        ]
        [ img
            [ src ("../images/die-face-" ++ dieValue ++ ".png")
            , alt dieValue
            , title dieValue
            , width 80
            ]
            []
        ]


{-| Show a representation of the provided die.
-}
clickableDieFaceImage : Die -> Html Msg
clickableDieFaceImage die =
    let
        dieValue =
            dieToString die
    in
    div
        [ style "float" "left"
        , style "cursor" "pointer"
        , style "user-select" "none"
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
        , style "user-select" "none"
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
        , style "user-select" "none"
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


gameActions : MultiSelectRing Die -> GameTurn -> Html Msg
gameActions dice turn =
    if turn == GameOver then
        button [ onClick GameRestarted ]
            [ text "Start over" ]

    else if MultiSelectRing.isNoneSelected dice then
        button [ onClick AllDiceScored ]
            [ text "Keep all" ]

    else
        let
            selectedCount =
                MultiSelectRing.countSelected dice
        in
        button [ onClick RollSelectedDice ]
            [ text ("Roll " ++ String.fromInt selectedCount) ]
