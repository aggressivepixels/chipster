module Main exposing (main)

import Browser
import Hex
import Html exposing (Html)
import Html.Attributes as Attributes
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as Decode exposing (Value)
import Time



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
    = Advance


init : Value -> ( Model, Cmd Msg )
init value =
    let
        model =
            case Decode.decodeValue Decode.int value of
                Ok seed ->
                    case Interpreter.init kaleid seed of
                        Just oldInterpreter ->
                            Running oldInterpreter

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
        Running oldInterpreter ->
            Interpreter.view oldInterpreter

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
        ( Running oldInterpreter, Advance ) ->
            let
                newModel =
                    case Interpreter.update oldInterpreter of
                        Ok newInterpreter ->
                            Running newInterpreter

                        Err error ->
                            Crashed error oldInterpreter
            in
            ( newModel
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Running _ ->
            Time.every 10 (\_ -> Advance)

        _ ->
            Sub.none



-- GAMES


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


kaleid : List Int
kaleid =
    [ 0x60
    , 0x00
    , 0x63
    , 0x80
    , 0x61
    , 0x1F
    , 0x62
    , 0x0F
    , 0x22
    , 0x32
    , 0xA2
    , 0x00
    , 0xF3
    , 0x1E
    , 0xF0
    , 0x0A
    , 0xF0
    , 0x55
    , 0x40
    , 0x00
    , 0x12
    , 0x1C
    , 0x73
    , 0x01
    , 0x33
    , 0x00
    , 0x12
    , 0x08
    , 0x63
    , 0x80
    , 0xA2
    , 0x00
    , 0xF3
    , 0x1E
    , 0xF0
    , 0x65
    , 0x40
    , 0x00
    , 0x12
    , 0x1C
    , 0x73
    , 0x01
    , 0x43
    , 0x00
    , 0x12
    , 0x1C
    , 0x22
    , 0x32
    , 0x12
    , 0x1E
    , 0x40
    , 0x02
    , 0x72
    , 0xFF
    , 0x40
    , 0x04
    , 0x71
    , 0xFF
    , 0x40
    , 0x06
    , 0x71
    , 0x01
    , 0x40
    , 0x08
    , 0x72
    , 0x01
    , 0xA2
    , 0x77
    , 0x6A
    , 0xE0
    , 0x8A
    , 0x12
    , 0x6B
    , 0x1F
    , 0x81
    , 0xB2
    , 0x3A
    , 0x00
    , 0x72
    , 0x01
    , 0x6A
    , 0xF0
    , 0x8A
    , 0x22
    , 0x6B
    , 0x0F
    , 0x82
    , 0xB2
    , 0x3A
    , 0x00
    , 0x71
    , 0x01
    , 0x6B
    , 0x1F
    , 0x81
    , 0xB2
    , 0xD1
    , 0x21
    , 0x8A
    , 0x10
    , 0x6B
    , 0x1F
    , 0x8B
    , 0x25
    , 0xDA
    , 0xB1
    , 0x6A
    , 0x3F
    , 0x8A
    , 0x15
    , 0xDA
    , 0xB1
    , 0x8B
    , 0x20
    , 0xDA
    , 0xB1
    , 0x00
    , 0xEE
    , 0x01
    , 0x80
    ]
