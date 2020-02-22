module Interpreter exposing (Interpreter, fromProgram, view)

import Array exposing (Array)
import Html exposing (Html)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type Interpreter
    = Interpreter Internals


type alias Internals =
    { memory : Array Int
    , programCounter : Int
    , display : Set Pixel
    }


type alias Pixel =
    ( Int, Int )


fromProgram : List Int -> Maybe Interpreter
fromProgram =
    internalsFromProgram >> Maybe.map Interpreter


internalsFromProgram : List Int -> Maybe Internals
internalsFromProgram program =
    if List.length program > 4096 - 512 then
        Nothing

    else
        Just
            { memory =
                Array.fromList
                    (List.concat
                        [ List.repeat 512 0
                        , program
                        , List.repeat (4096 - 512 - List.length program) 0
                        ]
                    )
            , programCounter = 512
            , display = Set.fromList [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
            }


view : Interpreter -> Html msg
view (Interpreter internals) =
    Svg.svg
        [ Attributes.width "640"
        , Attributes.height "320"
        , Attributes.viewBox "0 0 64 32"
        ]
        [ viewBackground
        , Svg.g [] (List.map viewPixel (Set.toList internals.display))
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
