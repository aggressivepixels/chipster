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
                    case Interpreter.init puzzle seed of
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


puzzle : List Int
puzzle =
    [ 0x6A
    , 0x12
    , 0x6B
    , 0x01
    , 0x61
    , 0x10
    , 0x62
    , 0x00
    , 0x60
    , 0x00
    , 0xA2
    , 0xB0
    , 0xD1
    , 0x27
    , 0xF0
    , 0x29
    , 0x30
    , 0x00
    , 0xDA
    , 0xB5
    , 0x71
    , 0x08
    , 0x7A
    , 0x08
    , 0x31
    , 0x30
    , 0x12
    , 0x24
    , 0x61
    , 0x10
    , 0x72
    , 0x08
    , 0x6A
    , 0x12
    , 0x7B
    , 0x08
    , 0xA3
    , 0x00
    , 0xF0
    , 0x1E
    , 0xF0
    , 0x55
    , 0x70
    , 0x01
    , 0x30
    , 0x10
    , 0x12
    , 0x0A
    , 0x6A
    , 0x12
    , 0x6B
    , 0x01
    , 0x6C
    , 0x00
    , 0x62
    , 0xFF
    , 0xC0
    , 0x06
    , 0x70
    , 0x02
    , 0x22
    , 0x52
    , 0x72
    , 0xFF
    , 0x32
    , 0x00
    , 0x12
    , 0x38
    , 0x6E
    , 0x00
    , 0x6E
    , 0x00
    , 0xF0
    , 0x0A
    , 0x22
    , 0x52
    , 0x7E
    , 0x01
    , 0x7E
    , 0x01
    , 0x12
    , 0x48
    , 0x84
    , 0xA0
    , 0x85
    , 0xB0
    , 0x86
    , 0xC0
    , 0x30
    , 0x02
    , 0x12
    , 0x64
    , 0x45
    , 0x01
    , 0x12
    , 0x64
    , 0x75
    , 0xF8
    , 0x76
    , 0xFC
    , 0x30
    , 0x08
    , 0x12
    , 0x70
    , 0x45
    , 0x19
    , 0x12
    , 0x70
    , 0x75
    , 0x08
    , 0x76
    , 0x04
    , 0x30
    , 0x06
    , 0x12
    , 0x7C
    , 0x44
    , 0x12
    , 0x12
    , 0x7C
    , 0x74
    , 0xF8
    , 0x76
    , 0xFF
    , 0x30
    , 0x04
    , 0x12
    , 0x88
    , 0x44
    , 0x2A
    , 0x12
    , 0x88
    , 0x74
    , 0x08
    , 0x76
    , 0x01
    , 0xA3
    , 0x00
    , 0xF6
    , 0x1E
    , 0xF0
    , 0x65
    , 0x81
    , 0x00
    , 0x60
    , 0x00
    , 0xA3
    , 0x00
    , 0xF6
    , 0x1E
    , 0xF0
    , 0x55
    , 0xA3
    , 0x00
    , 0xFC
    , 0x1E
    , 0x80
    , 0x10
    , 0xF0
    , 0x55
    , 0xF1
    , 0x29
    , 0xD4
    , 0x55
    , 0xDA
    , 0xB5
    , 0x8A
    , 0x40
    , 0x8B
    , 0x50
    , 0x8C
    , 0x60
    , 0x00
    , 0xEE
    , 0xEE
    , 0x5E
    , 0xFE
    , 0xFE
    , 0xFE
    , 0xFE
    , 0xFE
    , 0xFE
    , 0xFE
    , 0xFE
    ]
