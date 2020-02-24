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
                    case Interpreter.init guess seed of
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


guess : List Int
guess =
    [ 0x6E
    , 0x01
    , 0x00
    , 0xE0
    , 0x6D
    , 0x01
    , 0x6A
    , 0x01
    , 0x6B
    , 0x01
    , 0x8C
    , 0xD0
    , 0x8C
    , 0xE2
    , 0x4C
    , 0x00
    , 0x12
    , 0x20
    , 0x88
    , 0xD0
    , 0x22
    , 0x3E
    , 0x3A
    , 0x40
    , 0x12
    , 0x20
    , 0x6A
    , 0x01
    , 0x7B
    , 0x06
    , 0x3C
    , 0x3F
    , 0x7D
    , 0x01
    , 0x3D
    , 0x3F
    , 0x12
    , 0x0A
    , 0xF0
    , 0x0A
    , 0x40
    , 0x05
    , 0x89
    , 0xE4
    , 0x8E
    , 0xE4
    , 0x3E
    , 0x40
    , 0x12
    , 0x02
    , 0x6A
    , 0x1C
    , 0x6B
    , 0x0D
    , 0x88
    , 0x90
    , 0x00
    , 0xE0
    , 0x22
    , 0x3E
    , 0x12
    , 0x3C
    , 0xA2
    , 0x94
    , 0xF8
    , 0x33
    , 0xF2
    , 0x65
    , 0x22
    , 0x54
    , 0xDA
    , 0xB5
    , 0x7A
    , 0x04
    , 0x81
    , 0x20
    , 0x22
    , 0x54
    , 0xDA
    , 0xB5
    , 0x7A
    , 0x05
    , 0x00
    , 0xEE
    , 0x83
    , 0x10
    , 0x83
    , 0x34
    , 0x83
    , 0x34
    , 0x83
    , 0x14
    , 0xA2
    , 0x62
    , 0xF3
    , 0x1E
    , 0x00
    , 0xEE
    , 0xE0
    , 0xA0
    , 0xA0
    , 0xA0
    , 0xE0
    , 0x40
    , 0x40
    , 0x40
    , 0x40
    , 0x40
    , 0xE0
    , 0x20
    , 0xE0
    , 0x80
    , 0xE0
    , 0xE0
    , 0x20
    , 0xE0
    , 0x20
    , 0xE0
    , 0xA0
    , 0xA0
    , 0xE0
    , 0x20
    , 0x20
    , 0xE0
    , 0x80
    , 0xE0
    , 0x20
    , 0xE0
    , 0xE0
    , 0x80
    , 0xE0
    , 0xA0
    , 0xE0
    , 0xE0
    , 0x20
    , 0x20
    , 0x20
    , 0x20
    , 0xE0
    , 0xA0
    , 0xE0
    , 0xA0
    , 0xE0
    , 0xE0
    , 0xA0
    , 0xE0
    , 0x20
    , 0xE0
    ]
