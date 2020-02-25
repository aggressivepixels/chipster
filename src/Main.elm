module Main exposing (main)

import Browser
import Hex
import Html exposing (Html)
import Html.Attributes as Attributes
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as Decode exposing (Decoder, Value)


flagsDecoder : Decoder ( Int, List Game )
flagsDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "seed" Decode.int)
        (Decode.field "games" (Decode.list gameDecoder))


type alias Game =
    { name : String
    , data : List Int
    }


gameDecoder : Decoder Game
gameDecoder =
    Decode.map2 Game
        (Decode.field "name" Decode.string)
        (Decode.field "data" (Decode.list Decode.int))



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
            case Decode.decodeValue flagsDecoder value of
                Ok ( seed, x :: _ ) ->
                    case Interpreter.init x.data seed of
                        Just interpreter ->
                            Running interpreter

                        Nothing ->
                            InvalidProgram

                Ok _ ->
                    InvalidProgram

                Err _ ->
                    InvalidSeed
    in
    ( model, Cmd.none )


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
