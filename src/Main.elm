module Main exposing (main)

import Browser
import Hex exposing (toHexString)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as Decode exposing (Decoder, Value, decodeValue)
import Task
import Time


type Flags
    = Flags Game (List Game)


flagsDecoder : Decoder Flags
flagsDecoder =
    Decode.oneOrMore Flags gameDecoder


type alias Game =
    { name : String
    , data : List Int
    }


gameDecoder : Decoder Game
gameDecoder =
    Decode.map2 Game
        (Decode.field "name" Decode.string)
        (Decode.field "data" (Decode.list Decode.int))


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Invalid
    | Valid (List Game) Page


type Page
    = Dashboard
    | InvalidProgram String
    | Playing String Interpreter Status


type Status
    = Running
    | Crashed Error


init : Value -> ( Model, Cmd Msg )
init value =
    case decodeValue flagsDecoder value of
        Ok (Flags x xs) ->
            ( Valid (x :: xs) Dashboard, Cmd.none )

        Err _ ->
            ( Invalid, Cmd.none )


type Msg
    = GameClicked Game
    | BackClicked
    | SeedGenerated Int Game
    | InterpreterMsg Interpreter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Valid _ Dashboard, GameClicked game ) ->
            ( model
            , Task.perform
                (\time ->
                    SeedGenerated (Time.posixToMillis time) game
                )
                Time.now
            )

        ( Valid games Dashboard, SeedGenerated seed game ) ->
            case Interpreter.init game.data seed of
                Just interpreter ->
                    ( Valid games (Playing game.name interpreter Running)
                    , Cmd.none
                    )

                Nothing ->
                    ( Valid games (InvalidProgram game.name)
                    , Cmd.none
                    )

        ( Valid games (Playing name oldInterpreter Running), InterpreterMsg interpreterMsg ) ->
            case Interpreter.update interpreterMsg oldInterpreter of
                Ok newInterpreter ->
                    ( Valid games (Playing name newInterpreter Running), Cmd.none )

                Err error ->
                    ( Valid games (Playing name oldInterpreter (Crashed error)), Cmd.none )

        ( Valid games _, BackClicked ) ->
            ( Valid games Dashboard, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Valid _ (Playing _ interpreter Running) ->
            Sub.map InterpreterMsg (Interpreter.subscriptions interpreter)

        _ ->
            Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        appName =
            "Chipster"

        skeleton title content =
            [ Html.main_ []
                [ Html.h2 [] [ Html.text title ]
                , Html.div [] content
                ]
            ]
    in
    case model of
        Invalid ->
            { title = appName
            , body =
                skeleton "An error has ocurred" []
            }

        Valid games Dashboard ->
            { title = appName
            , body =
                skeleton appName
                    [ Html.p
                        [ Attributes.class "games" ]
                        (List.map viewGame games)
                    ]
            }

        Valid _ (InvalidProgram name) ->
            { title = name ++ " — " ++ appName
            , body =
                skeleton "Invalid program"
                    [ viewBack
                    , Html.text "That program seems to be invalid, try another one perhaps?"
                    ]
            }

        Valid _ (Playing name interpreter status) ->
            { title = name ++ " — " ++ appName
            , body =
                skeleton name
                    [ viewBack
                    , Html.div [ Attributes.class "interpreter" ]
                        [ Interpreter.view interpreter
                        ]
                    , case status of
                        Running ->
                            Html.text ""

                        Crashed (InvalidInstruction instruction) ->
                            Html.p []
                                [ Html.text
                                    ("Invalid instruction: "
                                        ++ toHexString instruction
                                    )
                                ]
                    ]
            }


viewBack : Html Msg
viewBack =
    Html.p []
        [ Html.a [ Attributes.href "#", Events.onClick BackClicked ]
            [ Html.text "Go back" ]
        ]


viewGame : Game -> Html Msg
viewGame game =
    Html.a
        [ Attributes.href "#"
        , Events.onClick (GameClicked game)
        ]
        [ Html.text game.name ]
