module Main exposing (main)

import Browser
import Html exposing (Html)
import Interpreter exposing (Interpreter)



-- TEA STUFF


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Running Interpreter
    | Crashed


type Msg
    = Msg


init : () -> ( Model, Cmd Msg )
init () =
    ( Maybe.map Running (Interpreter.fromProgram [ 0 ])
        |> Maybe.withDefault Crashed
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model of
        Running interpreter ->
            Interpreter.view interpreter

        Crashed ->
            Html.text "The program was too big for the interpreter's memory"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
