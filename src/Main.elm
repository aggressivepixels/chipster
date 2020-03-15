module Main exposing (main)

import Browser exposing (Document)
import Bytes
import Bytes.Decode as BD
import File exposing (File)
import File.Select as Select
import Hex exposing (toHexString)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Interpreter exposing (Error(..), Interpreter)
import Json.Decode as JD
import Task


fileBytes : Int -> BD.Decoder (List Int)
fileBytes length =
    BD.loop ( length, [] ) fileBytesStep


fileBytesStep :
    ( Int, List Int )
    -> BD.Decoder (BD.Step ( Int, List Int ) (List Int))
fileBytesStep ( n, xs ) =
    if n <= 0 then
        BD.succeed (BD.Done (List.reverse xs))

    else
        BD.map (\x -> BD.Loop ( n - 1, x :: xs )) BD.unsignedInt8


type alias Game =
    { name : String
    , data : List Int
    }


gameDecoder : JD.Decoder Game
gameDecoder =
    JD.map2 Game
        (JD.field "name" JD.string)
        (JD.field "data" (JD.list JD.int))


main : Program JD.Value Model Msg
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


init : JD.Value -> ( Model, Cmd Msg )
init value =
    case JD.decodeValue (JD.list gameDecoder) value of
        Ok games ->
            ( Valid games Dashboard, Cmd.none )

        Err _ ->
            ( Invalid, Cmd.none )


type Msg
    = GameClicked Game
    | BackClicked
    | LoadGameClicked
    | GameLoaded File
    | GameDecoded (Maybe Game)
    | GotInterpreter String (Result Interpreter.InvalidProgram Interpreter)
    | InterpreterMsg Interpreter.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Valid _ Dashboard, GameClicked game ) ->
            ( model
            , Task.attempt
                (GotInterpreter game.name)
                (Interpreter.make game.data)
            )

        ( Valid games Dashboard, GotInterpreter name (Ok interpreter) ) ->
            ( Valid games (Playing name interpreter Running)
            , Cmd.none
            )

        ( Valid games Dashboard, GotInterpreter name (Err _) ) ->
            ( Valid games (InvalidProgram name)
            , Cmd.none
            )

        ( Valid games (Playing name oldInterpreter Running), InterpreterMsg interpreterMsg ) ->
            case Interpreter.update interpreterMsg oldInterpreter of
                Ok ( newInterpreter, interpreterCmd ) ->
                    ( Valid games (Playing name newInterpreter Running)
                    , Cmd.map InterpreterMsg interpreterCmd
                    )

                Err error ->
                    ( Valid games (Playing name oldInterpreter (Crashed error))
                    , Cmd.none
                    )

        ( Valid games _, BackClicked ) ->
            ( Valid games Dashboard, Cmd.none )

        ( Valid _ _, LoadGameClicked ) ->
            ( model, Select.file [] GameLoaded )

        ( Valid _ _, GameLoaded game ) ->
            ( model
            , File.toBytes game
                |> Task.map
                    (\bytes ->
                        BD.decode (fileBytes (Bytes.width bytes)) bytes
                            |> Maybe.map (\data -> Game (File.name game) data)
                    )
                |> Task.perform GameDecoded
            )

        ( Valid _ _, GameDecoded (Just game) ) ->
            update (GameClicked game) model

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Valid _ (Playing _ interpreter Running) ->
            Sub.map InterpreterMsg (Interpreter.subscriptions interpreter)

        _ ->
            Sub.none


view : Model -> Document Msg
view model =
    let
        appName =
            "Chipster"

        skeleton title content =
            [ H.main_ []
                [ H.h2 [] [ H.text title ]
                , H.div [] content
                ]
            ]
    in
    case model of
        Invalid ->
            { title = appName
            , body = skeleton "An error has ocurred" []
            }

        Valid games Dashboard ->
            { title = appName
            , body =
                skeleton appName
                    [ H.p [] [ H.text "Choose a game" ]
                    , H.div
                        [ HA.class "games" ]
                        (List.map viewGame games)
                    , H.p []
                        [ H.text "Or "
                        , H.a
                            [ HA.href "#", HE.onClick LoadGameClicked ]
                            [ H.text "load your own" ]
                        ]
                    ]
            }

        Valid _ (InvalidProgram name) ->
            { title = name ++ " — " ++ appName
            , body =
                skeleton "Invalid program"
                    [ viewBack
                    , H.text "That program seems to be invalid, try another one perhaps?"
                    ]
            }

        Valid _ (Playing name interpreter status) ->
            { title = name ++ " — " ++ appName
            , body =
                skeleton name
                    [ viewBack
                    , H.div [ HA.class "interpreter" ]
                        [ Interpreter.view interpreter ]
                    , case status of
                        Running ->
                            H.text ""

                        Crashed (InvalidInstruction instruction) ->
                            H.p []
                                [ H.text
                                    ("Invalid instruction: "
                                        ++ toHexString instruction
                                    )
                                ]
                    ]
            }


viewBack : H.Html Msg
viewBack =
    H.p []
        [ H.a [ HA.href "#", HE.onClick BackClicked ]
            [ H.text "Go back" ]
        ]


viewGame : Game -> H.Html Msg
viewGame game =
    H.a [ HA.href "#", HE.onClick (GameClicked game) ] [ H.text game.name ]
