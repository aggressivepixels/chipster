module Interpreter exposing
    ( Error(..)
    , Interpreter
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Bitwise
import Browser.Events as Events
import Html exposing (Html)
import List.Extra
import Memory exposing (Address(..), Memory)
import Random exposing (Generator, Seed)
import Registers exposing (Registers)
import Set exposing (Set)
import Stack exposing (Stack)
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
    , registers : Registers
    , stack : Stack
    }


type alias Pixel =
    ( Int, Int )


type Error
    = InvalidInstruction Int


type Msg
    = FramePassed Float



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
                    , registers = Registers.init
                    , display = Set.empty
                    , seed = Random.initialSeed seed
                    , stack = Stack.init
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


update : Msg -> Interpreter -> Result Error Interpreter
update msg (Interpreter internals) =
    case msg of
        FramePassed _ ->
            fetchInstruction internals
                |> runInstruction internals
                |> Result.map Interpreter


fetchInstruction : Internals -> Int
fetchInstruction internals =
    let
        high =
            Memory.read (Address internals.programCounter) internals.memory
                |> Bitwise.shiftLeftBy 8

        low =
            Memory.read (Address (internals.programCounter + 1)) internals.memory
    in
    Bitwise.or high low


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
        0x00 ->
            case kk of
                0xE0 ->
                    Ok
                        { internals
                            | display = Set.empty
                            , programCounter = internals.programCounter + 2
                        }

                0xEE ->
                    let
                        ( newProgramCounter, newStack ) =
                            Stack.pop internals.stack
                    in
                    Ok
                        { internals
                            | stack = newStack
                            , programCounter = newProgramCounter + 2
                        }

                _ ->
                    Err (InvalidInstruction instruction)

        -- 1nnn - JP nnn
        0x01 ->
            Ok { internals | programCounter = nnn }

        -- 2nnn - CALL nnn
        0x02 ->
            Ok
                { internals
                    | programCounter = nnn
                    , stack = Stack.push internals.programCounter internals.stack
                }

        -- 3xkk - SE Vx, kk
        0x03 ->
            Ok
                { internals
                    | programCounter =
                        internals.programCounter
                            + (if Registers.get x internals.registers == kk then
                                4

                               else
                                2
                              )
                }

        -- 4xkk - SNE Vx, kk
        0x04 ->
            Ok
                { internals
                    | programCounter =
                        internals.programCounter
                            + (if Registers.get x internals.registers /= kk then
                                4

                               else
                                2
                              )
                }

        -- 6xkk - LD Vx, kk
        0x06 ->
            Ok
                { internals
                    | registers = Registers.set x kk internals.registers
                    , programCounter = internals.programCounter + 2
                }

        -- 7xkk - ADD Vx, kk
        0x07 ->
            Ok
                { internals
                    | registers =
                        Registers.modify
                            x
                            ((+) kk >> modBy 256)
                            internals.registers
                    , programCounter = internals.programCounter + 2
                }

        0x08 ->
            case n of
                0x00 ->
                    Ok
                        { internals
                            | registers =
                                Registers.set x
                                    (Registers.get y internals.registers)
                                    internals.registers
                            , programCounter = internals.programCounter + 2
                        }

                0x02 ->
                    Ok
                        { internals
                            | registers =
                                Registers.set x
                                    (Bitwise.and
                                        (Registers.get x internals.registers)
                                        (Registers.get y internals.registers)
                                    )
                                    internals.registers
                            , programCounter = internals.programCounter + 2
                        }

                0x05 ->
                    let
                        result =
                            Registers.get x internals.registers
                                - Registers.get y internals.registers
                    in
                    Ok
                        { internals
                            | registers =
                                internals.registers
                                    |> Registers.set x (modBy 256 result)
                                    |> Registers.set 0x0F
                                        (if result > 0 then
                                            1

                                         else
                                            0
                                        )
                            , programCounter = internals.programCounter + 2
                        }

                _ ->
                    Err (InvalidInstruction instruction)

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
                        Registers.set x
                            (Bitwise.and kk randomByte)
                            internals.registers
                    , programCounter = internals.programCounter + 2
                }

        -- Dxyn - DRW Vx, Vy, n
        0x0D ->
            let
                rows =
                    Memory.readMany n
                        (Address internals.indexRegister)
                        internals.memory

                sprite =
                    spriteFromRows rows
                        |> List.map
                            (Tuple.mapBoth
                                ((+) (Registers.get x internals.registers))
                                ((+) (Registers.get y internals.registers))
                            )
                        |> Set.fromList

                union =
                    Set.union sprite internals.display
            in
            -- TODO: Broken! Doesn't handle collisions.
            Ok
                { internals
                    | display = union
                    , programCounter = internals.programCounter + 2
                }

        0x0F ->
            case kk of
                0x1E ->
                    Ok
                        { internals
                            | indexRegister =
                                Registers.get x internals.registers
                                    + internals.indexRegister
                            , programCounter = internals.programCounter + 2
                        }

                _ ->
                    Err (InvalidInstruction instruction)

        _ ->
            Err (InvalidInstruction instruction)


spriteFromRows : List Int -> List Pixel
spriteFromRows =
    List.Extra.indexedFoldl
        (\y row spritePixels ->
            spritePixels
                ++ List.Extra.indexedFoldl
                    (\x isOn rowPixels ->
                        if isOn then
                            ( x, y ) :: rowPixels

                        else
                            rowPixels
                    )
                    []
                    (getBits 8 row)
        )
        []


getBits : Int -> Int -> List Bool
getBits =
    getBitsHelp []


getBitsHelp : List Bool -> Int -> Int -> List Bool
getBitsHelp spritePixels count int =
    if count <= 0 then
        List.reverse spritePixels

    else
        let
            mask =
                Bitwise.shiftLeftBy (count - 1) 1

            isOn =
                Bitwise.and mask int /= 0
        in
        getBitsHelp (isOn :: spritePixels) (count - 1) int


randomByteGenerator : Generator Int
randomByteGenerator =
    Random.int 0 255



-- SUBSCRIPTIONS


subscriptions : Interpreter -> Sub Msg
subscriptions _ =
    Sub.batch [ Events.onAnimationFrameDelta FramePassed ]
