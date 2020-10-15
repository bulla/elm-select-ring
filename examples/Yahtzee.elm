module DiceSelect exposing (..)

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
    , selectedScore : Int
    , turn : GameTurn
    }


type Die
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


type GameTurn
    = FirstTurn
    | SecondTurn
    | ThirdTurn


init : () -> ( Model, Cmd Msg )
init _ =
    ( { dice = MultiSelectRing.empty
      , score = 0
      , selectedScore = 0
      , turn = FirstTurn
      }
    , Random.generate DiceRolled (randomDiceGenerator 5)
    )



-- UPDATE


type Msg
    = RollDice
    | DiceRolled (List Die)
    | DieClicked Die
    | ButtonPressed Button
    | ScoreSelectedDiceAndRollRemaining Int
    | AllRemainingDiceScored


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

        DiceRolled dice ->
            ( rollDice dice model
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

        ScoreSelectedDiceAndRollRemaining remainingDice ->
            ( model
            , Random.generate DiceRolled (randomDiceGenerator remainingDice)
            )

        AllRemainingDiceScored ->
            ( finalizeScore model
            , Cmd.none
            )


randomDiceGenerator : Int -> Random.Generator (List Die)
randomDiceGenerator count =
    Random.int 1 6
        |> Random.list count
        |> Random.map (\dice -> List.map intToDie dice)


rollDice : List Die -> Model -> Model
rollDice dice model =
    { model
        | dice = MultiSelectRing.fromList dice
    }


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
toggleDie toggledItem model =
    { model
        | dice =
            model.dice
                |> MultiSelectRing.focusOnFirstMatching (\item -> item == toggledItem)
                |> MultiSelectRing.toggleFocused
    }


{-| Toggle selection of the currently focused item.
-}
toggleFocused : Model -> Model
toggleFocused model =
    let
        dice =
            MultiSelectRing.toggleFocused model.dice
    in
    { model
        | dice = dice
        , selectedScore = sumSelectedDice dice
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
        | dice = MultiSelectRing.empty
        , score = sumSelectedDice model.dice
    }


sumSelectedDice : MultiSelectRing Die -> Int
sumSelectedDice dice =
    MultiSelectRing.getSelected dice
        |> List.map dieToInt
        |> List.sum



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
            [ text "Pick a few dice" ]
        , div [ style "padding" "8px 12px" ]
            [ text ("Score: " ++ String.fromInt model.score) ]
        , div [ style "padding" "0 12px" ]
            [ diceSelectorView model.dice ]
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ diceActions model.dice ]
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


{-| Show a still representation of the provided item.
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


{-| Show a representation of the selected item that has been provided.
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


intToDie : Int -> Die
intToDie value =
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


dieToInt : Die -> Int
dieToInt die =
    case die of
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


dieToString : Die -> String
dieToString die =
    String.fromInt (dieToInt die)


diceActions : MultiSelectRing Die -> Html Msg
diceActions dice =
    if MultiSelectRing.isNoneSelected dice then
        button [ onClick RollDice ] [ text "Roll again" ]

    else if MultiSelectRing.isAllSelected dice then
        button [ onClick AllRemainingDiceScored ] [ text "Score all" ]

    else
        button [ onClick (ScoreSelectedDiceAndRollRemaining 2) ] [ text "Score and re-roll" ]
