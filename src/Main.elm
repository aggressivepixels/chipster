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
                    case Interpreter.init connect4 seed of
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


connect4 : List Int
connect4 =
    [ 0x12
    , 0x1A
    , 0x43
    , 0x4F
    , 0x4E
    , 0x4E
    , 0x45
    , 0x43
    , 0x54
    , 0x34
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
    , 0xA2
    , 0xBB
    , 0xF6
    , 0x65
    , 0xA2
    , 0xB4
    , 0xF6
    , 0x55
    , 0x69
    , 0x00
    , 0x68
    , 0x01
    , 0x6B
    , 0x00
    , 0x6D
    , 0x0F
    , 0x6E
    , 0x1F
    , 0xA2
    , 0xA5
    , 0x60
    , 0x0D
    , 0x61
    , 0x32
    , 0x62
    , 0x00
    , 0xD0
    , 0x2F
    , 0xD1
    , 0x2F
    , 0x72
    , 0x0F
    , 0x32
    , 0x1E
    , 0x12
    , 0x34
    , 0xD0
    , 0x21
    , 0xD1
    , 0x21
    , 0x72
    , 0x01
    , 0x60
    , 0x0A
    , 0xA2
    , 0x9F
    , 0xD0
    , 0x21
    , 0xD1
    , 0x21
    , 0xA2
    , 0x9F
    , 0xDD
    , 0xE1
    , 0xFC
    , 0x0A
    , 0xDD
    , 0xE1
    , 0x4C
    , 0x05
    , 0x12
    , 0x7E
    , 0x3C
    , 0x04
    , 0x12
    , 0x6A
    , 0x7B
    , 0xFF
    , 0x7D
    , 0xFB
    , 0x3D
    , 0x0A
    , 0x12
    , 0x7A
    , 0x6B
    , 0x06
    , 0x6D
    , 0x2D
    , 0x12
    , 0x7A
    , 0x3C
    , 0x06
    , 0x12
    , 0x98
    , 0x7B
    , 0x01
    , 0x7D
    , 0x05
    , 0x3D
    , 0x32
    , 0x12
    , 0x7A
    , 0x6B
    , 0x00
    , 0x6D
    , 0x0F
    , 0xDD
    , 0xE1
    , 0x12
    , 0x50
    , 0xA2
    , 0xB4
    , 0xFB
    , 0x1E
    , 0xF0
    , 0x65
    , 0x40
    , 0xFC
    , 0x12
    , 0x98
    , 0x8A
    , 0x00
    , 0x70
    , 0xFB
    , 0xF0
    , 0x55
    , 0x89
    , 0x83
    , 0xA2
    , 0x9E
    , 0x39
    , 0x00
    , 0xA2
    , 0xA1
    , 0xDD
    , 0xA4
    , 0xA2
    , 0x9F
    , 0xDD
    , 0xE1
    , 0x12
    , 0x50
    , 0x60
    , 0xF0
    , 0xF0
    , 0x60
    , 0x90
    , 0x90
    , 0x60
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x80
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    , 0x1A
    ]
