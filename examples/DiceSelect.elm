module DiceSelect exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (int)



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
    { dice : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model []
    , Random.generate DiceRolled fiveRandomDice
    )



-- UPDATE


type Msg
    = RollDice
    | DiceRolled (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RollDice ->
            ( model
            , Random.generate DiceRolled fiveRandomDice
            )

        DiceRolled dice ->
            ( { dice = dice }
            , Cmd.none
            )


fiveRandomDice : Random.Generator (List Int)
fiveRandomDice =
    Random.list 5 (Random.int 1 6)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style "padding" "12px" ]
            [ text "Pick a few dice" ]
        , div [ style "padding" "0 12px" ]
            (List.map dieFaceImage model.dice)
        , div
            [ style "clear" "both"
            , style "padding" "20px 12px"
            ]
            [ button [ onClick RollDice ] [ text "Roll" ] ]
        ]


dieFaceImage : Int -> Html Msg
dieFaceImage dieFace =
    let
        dieFaceImagePath =
            "../images/die-face-" ++ String.fromInt dieFace ++ ".jpg"
    in
    div [ style "float" "left" ]
        [ img
            [ src dieFaceImagePath
            , alt (String.fromInt dieFace)
            , width 80
            ]
            []
        ]
