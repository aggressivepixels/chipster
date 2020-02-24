module Main exposing (main)

import Browser
import Hex
import Html exposing (Html)
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as Decode exposing (Value)
import Time


maze : List Int
maze =
    [ 0xA2
    , 0x1E
    , 0xC2
    , 0x01
    , 0x32
    , 0x01
    , 0xA2
    , 0x1A
    , 0xD0
    , 0x14
    , 0x70
    , 0x04
    , 0x30
    , 0x40
    , 0x12
    , 0x00
    , 0x60
    , 0x00
    , 0x71
    , 0x04
    , 0x31
    , 0x20
    , 0x12
    , 0x00
    , 0x12
    , 0x18
    , 0x80
    , 0x40
    , 0x20
    , 0x10
    , 0x20
    , 0x40
    , 0x80
    , 0x10
    ]



-- TEA STUFF


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Error
    = NoInitialSeed
    | InvalidProgram
    | InterpreterError Interpreter.Error


type alias Model =
    Result Error Interpreter


type Msg
    = Advance


init : Value -> ( Model, Cmd Msg )
init value =
    ( Decode.decodeValue Decode.int value
        |> Result.mapError
            (\_ -> NoInitialSeed)
        |> Result.andThen
            (Interpreter.init maze >> Result.fromMaybe InvalidProgram)
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model of
        Ok interpreter ->
            Interpreter.view interpreter

        Err InvalidProgram ->
            Html.text "The program seems to be invalid"

        Err NoInitialSeed ->
            Html.text "The initial seed was not provided"

        Err (InterpreterError (InvalidInstruction instruction)) ->
            Html.text
                ("The program attempted to execute an invalid instruction: "
                    ++ Hex.toHexString instruction
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Ok interpreter, Advance ) ->
            ( Result.mapError InterpreterError (Interpreter.update interpreter)
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 (\_ -> Advance)
