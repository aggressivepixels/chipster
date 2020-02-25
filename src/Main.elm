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
                    case Interpreter.init wipeoff seed of
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


wipeoff : List Int
wipeoff =
    [ 0xA2
    , 0xCC
    , 0x6A
    , 0x07
    , 0x61
    , 0x00
    , 0x6B
    , 0x08
    , 0x60
    , 0x00
    , 0xD0
    , 0x11
    , 0x70
    , 0x08
    , 0x7B
    , 0xFF
    , 0x3B
    , 0x00
    , 0x12
    , 0x0A
    , 0x71
    , 0x04
    , 0x7A
    , 0xFF
    , 0x3A
    , 0x00
    , 0x12
    , 0x06
    , 0x66
    , 0x00
    , 0x67
    , 0x10
    , 0xA2
    , 0xCD
    , 0x60
    , 0x20
    , 0x61
    , 0x1E
    , 0xD0
    , 0x11
    , 0x63
    , 0x1D
    , 0x62
    , 0x3F
    , 0x82
    , 0x02
    , 0x77
    , 0xFF
    , 0x47
    , 0x00
    , 0x12
    , 0xAA
    , 0xFF
    , 0x0A
    , 0xA2
    , 0xCB
    , 0xD2
    , 0x31
    , 0x65
    , 0xFF
    , 0xC4
    , 0x01
    , 0x34
    , 0x01
    , 0x64
    , 0xFF
    , 0xA2
    , 0xCD
    , 0x6C
    , 0x00
    , 0x6E
    , 0x04
    , 0xEE
    , 0xA1
    , 0x6C
    , 0xFF
    , 0x6E
    , 0x06
    , 0xEE
    , 0xA1
    , 0x6C
    , 0x01
    , 0xD0
    , 0x11
    , 0x80
    , 0xC4
    , 0xD0
    , 0x11
    , 0x4F
    , 0x01
    , 0x12
    , 0x98
    , 0x42
    , 0x00
    , 0x64
    , 0x01
    , 0x42
    , 0x3F
    , 0x64
    , 0xFF
    , 0x43
    , 0x00
    , 0x65
    , 0x01
    , 0x43
    , 0x1F
    , 0x12
    , 0xA4
    , 0xA2
    , 0xCB
    , 0xD2
    , 0x31
    , 0x82
    , 0x44
    , 0x83
    , 0x54
    , 0xD2
    , 0x31
    , 0x3F
    , 0x01
    , 0x12
    , 0x42
    , 0x43
    , 0x1E
    , 0x12
    , 0x98
    , 0x6A
    , 0x02
    , 0xFA
    , 0x18
    , 0x76
    , 0x01
    , 0x46
    , 0x70
    , 0x12
    , 0xAA
    , 0xD2
    , 0x31
    , 0xC4
    , 0x01
    , 0x34
    , 0x01
    , 0x64
    , 0xFF
    , 0xC5
    , 0x01
    , 0x35
    , 0x01
    , 0x65
    , 0xFF
    , 0x12
    , 0x42
    , 0x6A
    , 0x03
    , 0xFA
    , 0x18
    , 0xA2
    , 0xCB
    , 0xD2
    , 0x31
    , 0x73
    , 0xFF
    , 0x12
    , 0x36
    , 0xA2
    , 0xCB
    , 0xD2
    , 0x31
    , 0x12
    , 0x28
    , 0xA2
    , 0xCD
    , 0xD0
    , 0x11
    , 0xA2
    , 0xF0
    , 0xF6
    , 0x33
    , 0xF2
    , 0x65
    , 0x63
    , 0x18
    , 0x64
    , 0x1B
    , 0xF0
    , 0x29
    , 0xD3
    , 0x45
    , 0x73
    , 0x05
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
    , 0xC8
    , 0x01
    , 0x80
    , 0x44
    , 0xFF
    ]
