module Main exposing (main)

import Browser
import Hex
import Html exposing (Html)
import Html.Attributes as Attributes
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as Decode exposing (Value)



-- TEA STUFF


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = InvalidSeed
    | InvalidProgram
    | Running Interpreter
    | Crashed Error Interpreter


type Msg
    = InterpreterMsg Interpreter.Msg


init : Value -> ( Model, Cmd Msg )
init value =
    let
        model =
            case Decode.decodeValue Decode.int value of
                Ok seed ->
                    case Interpreter.init missile seed of
                        Just interpreter ->
                            Running interpreter

                        Nothing ->
                            InvalidProgram

                Err _ ->
                    InvalidSeed
    in
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model of
        Running interpreter ->
            Interpreter.view interpreter

        InvalidProgram ->
            Html.text "The program seems to be invalid"

        InvalidSeed ->
            Html.text "The initial seed was not provided or was not an integer"

        Crashed (InvalidInstruction instruction) oldInterpreter ->
            Html.div []
                [ Html.div [ Attributes.style "display" "block" ]
                    [ Interpreter.view oldInterpreter ]
                , Html.text
                    ("The program attempted to execute an invalid instruction: "
                        ++ Hex.toHexString instruction
                    )
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Running oldInterpreter, InterpreterMsg interpreterMsg ) ->
            let
                newModel =
                    case Interpreter.update interpreterMsg oldInterpreter of
                        Ok newInterpreter ->
                            Running newInterpreter

                        Err error ->
                            Crashed error oldInterpreter
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running interpreter ->
            Sub.map InterpreterMsg (Interpreter.subscriptions interpreter)

        _ ->
            Sub.none



-- GAMES


missile : List Int
missile =
    [ 0x12
    , 0x19
    , 0x4D
    , 0x49
    , 0x53
    , 0x53
    , 0x49
    , 0x4C
    , 0x45
    , 0x20
    , 0x62
    , 0x79
    , 0x20
    , 0x44
    , 0x61
    , 0x76
    , 0x69
    , 0x64
    , 0x20
    , 0x57
    , 0x49
    , 0x4E
    , 0x54
    , 0x45
    , 0x52
    , 0x6C
    , 0x0C
    , 0x60
    , 0x00
    , 0x61
    , 0x00
    , 0x65
    , 0x08
    , 0x66
    , 0x0A
    , 0x67
    , 0x00
    , 0x6E
    , 0x01
    , 0xA2
    , 0xAD
    , 0xD0
    , 0x14
    , 0x70
    , 0x08
    , 0x30
    , 0x40
    , 0x12
    , 0x29
    , 0x60
    , 0x00
    , 0x61
    , 0x1C
    , 0xA2
    , 0xB0
    , 0xD0
    , 0x14
    , 0xA2
    , 0xB0
    , 0xD0
    , 0x14
    , 0x3E
    , 0x01
    , 0x12
    , 0x49
    , 0x70
    , 0x04
    , 0x40
    , 0x38
    , 0x6E
    , 0x00
    , 0x12
    , 0x4F
    , 0x70
    , 0xFC
    , 0x40
    , 0x00
    , 0x6E
    , 0x01
    , 0xD0
    , 0x14
    , 0xFC
    , 0x15
    , 0xFB
    , 0x07
    , 0x3B
    , 0x00
    , 0x12
    , 0x53
    , 0x62
    , 0x08
    , 0xE2
    , 0x9E
    , 0x12
    , 0x95
    , 0x3C
    , 0x00
    , 0x7C
    , 0xFE
    , 0x63
    , 0x1B
    , 0x82
    , 0x00
    , 0xA2
    , 0xB0
    , 0xD2
    , 0x31
    , 0x64
    , 0x00
    , 0xD2
    , 0x31
    , 0x73
    , 0xFF
    , 0xD2
    , 0x31
    , 0x3F
    , 0x00
    , 0x64
    , 0x01
    , 0x33
    , 0x03
    , 0x12
    , 0x6D
    , 0xD2
    , 0x31
    , 0x34
    , 0x01
    , 0x12
    , 0x91
    , 0x77
    , 0x05
    , 0x75
    , 0xFF
    , 0x82
    , 0x00
    , 0x63
    , 0x00
    , 0xA2
    , 0xAD
    , 0xD2
    , 0x34
    , 0x45
    , 0x00
    , 0x12
    , 0x97
    , 0x76
    , 0xFF
    , 0x36
    , 0x00
    , 0x12
    , 0x39
    , 0xA2
    , 0xB4
    , 0xF7
    , 0x33
    , 0xF2
    , 0x65
    , 0x63
    , 0x1B
    , 0x64
    , 0x0D
    , 0xF1
    , 0x29
    , 0xD3
    , 0x45
    , 0x73
    , 0x05
    , 0xF2
    , 0x29
    , 0xD3
    , 0x45
    , 0x12
    , 0xAB
    , 0x10
    , 0x38
    , 0x38
    , 0x10
    , 0x38
    , 0x7C
    , 0xFE
    ]
