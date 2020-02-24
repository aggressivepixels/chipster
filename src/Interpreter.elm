module Interpreter exposing
    ( Error(..)
    , Interpreter
    , init
    , update
    , view
    )

import Array exposing (Array)
import Bitwise
import Html exposing (Html)
import Memory exposing (Address(..), Memory)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed as Keyed
import Svg.Lazy exposing (lazy)


type Interpreter
    = Interpreter Internals


type alias Internals =
    { memory : Memory
    , programCounter : Int
    , indexRegister : Int
    , display : Set Pixel
    , seed : Seed
    , registers : Array Int
    }


type alias Pixel =
    ( Int, Int )


type Error
    = MemoryOutOfBounds
    | InvalidInstruction Int
    | InvalidRegister



-- INIT


init : List Int -> Int -> Maybe Interpreter
init program seed =
    Memory.init program
        |> Maybe.map
            (\memory ->
                Interpreter
                    { memory = memory
                    , programCounter = 512
                    , indexRegister = 0
                    , registers = Array.repeat 16 0
                    , display = Set.empty
                    , seed = Random.initialSeed seed
                    }
            )



-- VIEW


view : Interpreter -> Html msg
view (Interpreter internals) =
    Svg.svg
        [ Attributes.width "640"
        , Attributes.height "320"
        , Attributes.viewBox "0 0 64 32"
        ]
        [ viewBackground
        , lazy viewDisplay internals.display
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


viewDisplay : Set Pixel -> Svg msg
viewDisplay =
    Set.toList >> List.map viewKeyedPixel >> Keyed.node "g" []


viewKeyedPixel : Pixel -> ( String, Svg msg )
viewKeyedPixel ( x, y ) =
    ( String.fromInt x ++ "," ++ String.fromInt y
    , lazy viewPixel ( x, y )
    )


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



-- UPDATE


update : Interpreter -> Result Error Interpreter
update (Interpreter internals) =
    fetchInstruction internals
        |> Result.fromMaybe MemoryOutOfBounds
        |> Result.andThen (runInstruction internals)
        |> Result.map Interpreter


fetchInstruction : Internals -> Maybe Int
fetchInstruction internals =
    let
        high =
            Memory.read (Address internals.programCounter) internals.memory
                |> Maybe.map (Bitwise.shiftLeftBy 8)

        low =
            Memory.read (Address (internals.programCounter + 1))
                internals.memory
    in
    Maybe.map2 Bitwise.or high low


runInstruction : Internals -> Int -> Result Error Internals
runInstruction internals instruction =
    let
        op =
            Bitwise.and instruction 0xF000
                |> Bitwise.shiftRightZfBy 12

        x =
            Bitwise.and instruction 0x0F00
                |> Bitwise.shiftRightZfBy 8

        y =
            Bitwise.and instruction 0xF0
                |> Bitwise.shiftRightZfBy 4

        n =
            Bitwise.and instruction 0x0F

        kk =
            Bitwise.and instruction 0xFF

        nnn =
            Bitwise.and instruction 0x0FFF
    in
    case op of
        -- 1nnn - JP nnn
        0x01 ->
            Ok { internals | programCounter = nnn }

        -- 3xkk - SE Vx, kk
        0x03 ->
            Array.get x internals.registers
                |> Result.fromMaybe InvalidRegister
                |> Result.map
                    (\val ->
                        { internals
                            | programCounter =
                                internals.programCounter
                                    + (if val == kk then
                                        4

                                       else
                                        2
                                      )
                        }
                    )

        -- 6xkk - LD Vx, kk
        0x06 ->
            Ok
                { internals
                    | registers = Array.set x kk internals.registers
                    , programCounter = internals.programCounter + 2
                }

        -- 7xkk - ADD Vx, kk
        0x07 ->
            Array.get x internals.registers
                |> Result.fromMaybe InvalidRegister
                |> Result.map
                    (\val ->
                        { internals
                            | registers =
                                Array.set x
                                    (modBy 256 (val + kk))
                                    internals.registers
                            , programCounter = internals.programCounter + 2
                        }
                    )

        -- Annn - LD I, nnn
        0x0A ->
            Ok
                { internals
                    | indexRegister = nnn
                    , programCounter = internals.programCounter + 2
                }

        -- Cxkk - RND Vx, kk
        0x0C ->
            let
                ( randomByte, newSeed ) =
                    Random.step randomByteGenerator internals.seed
            in
            Ok
                { internals
                    | seed = newSeed
                    , registers =
                        Array.set x
                            (Bitwise.and kk randomByte)
                            internals.registers
                    , programCounter = internals.programCounter + 2
                }

        -- Dxyn - DRW Vx, Vy, n
        0x0D ->
            drawSprite x y n internals

        _ ->
            Err (InvalidInstruction instruction)


drawSprite : Int -> Int -> Int -> Internals -> Result Error Internals
drawSprite x y n internals =
    Memory.readMany n (Address internals.indexRegister) internals.memory
        |> Result.fromMaybe MemoryOutOfBounds
        |> Result.map
            (\rows ->
                let
                    sprite =
                        spriteFromRows rows
                            |> List.map
                                (Tuple.mapBoth
                                    ((+) (getWithDefault 0 x internals.registers))
                                    ((+) (getWithDefault 0 y internals.registers))
                                )
                            |> Set.fromList

                    union =
                        Set.union sprite internals.display
                in
                -- TODO: Broken! Doesn't handle collision.
                { internals
                    | display = union
                    , programCounter = internals.programCounter + 2
                }
            )


getWithDefault : a -> Int -> Array a -> a
getWithDefault default index array =
    Maybe.withDefault default (Array.get index array)


spriteFromRows : List Int -> List Pixel
spriteFromRows rows =
    -- TODO: Optimize this.
    rows
        |> List.indexedMap
            (\x row ->
                ( x
                , List.indexedMap Tuple.pair (getBits 8 row)
                )
            )
        |> List.concatMap
            (\( x, row ) ->
                List.map (\( y, on ) -> ( x, y, on )) row
            )
        |> List.filterMap
            (\( x, y, on ) ->
                if on then
                    Just ( x, y )

                else
                    Nothing
            )


getBits : Int -> Int -> List Bool
getBits =
    getBitsHelp []


getBitsHelp : List Bool -> Int -> Int -> List Bool
getBitsHelp soFar count int =
    if count <= 0 then
        List.reverse soFar

    else
        let
            mask =
                Bitwise.shiftLeftBy (count - 1) 1

            bit =
                Bitwise.and mask int /= 0
        in
        getBitsHelp (bit :: soFar) (count - 1) int


randomByteGenerator : Generator Int
randomByteGenerator =
    Random.int 0 255
