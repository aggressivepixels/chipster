module Main exposing (main)

import Browser
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Attributes



-- TEA STUFF


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { display : Display }


type Msg
    = Msg


init : () -> ( Model, Cmd Msg )
init () =
    ( { display = Set.fromList [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ] }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    viewDisplay model.display


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- DISPLAY


type alias Display =
    Set Pixel


type alias Pixel =
    ( Int, Int )


viewDisplay : Display -> Html msg
viewDisplay display =
    Svg.svg
        [ Attributes.width "640"
        , Attributes.height "320"
        , Attributes.viewBox "0 0 64 32"
        ]
        [ viewBackground
        , Svg.g [] (List.map viewPixel (Set.toList display))
        ]


viewBackground : Svg msg
viewBackground =
    Svg.rect
        [ Attributes.x "0"
        , Attributes.y "0"
        , Attributes.width "64"
        , Attributes.height "32"
        , Attributes.fill "black"
        ]
        []


viewPixel : Pixel -> Svg msg
viewPixel ( x, y ) =
    Svg.rect
        [ Attributes.x (String.fromInt x)
        , Attributes.y (String.fromInt y)
        , Attributes.width "1"
        , Attributes.height "1"
        , Attributes.fill "white"
        ]
        []
