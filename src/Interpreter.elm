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
import Json.Decode as Decode exposing (Decoder)
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
    = Interpreter State


type alias State =
    { memory : Memory
    , programCounter : Int
    , indexRegister : Int
    , display : Set Pixel
    , seed : Seed
    , registers : Registers
    , stack : Stack
    , status : Status
    , keypad : Set Int
    , delayTimer : Int
    , soundTimer : Int
    }


type alias Pixel =
    ( Int, Int )


type Status
    = Running
    | WaitingForInput Int


type Error
    = InvalidInstruction Int


type Msg
    = FramePassed Float
    | KeyPressed Int
    | KeyReleased Int



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
                    , status = Running
                    , keypad = Set.empty
                    , delayTimer = 0
                    , soundTimer = 0
                    }
            )



-- VIEW


view : Interpreter -> Html msg
view (Interpreter state) =
    Svg.svg
        [ Attributes.width "640"
        , Attributes.height "320"
        , Attributes.viewBox "0 0 64 32"
        ]
        [ viewBackground
        , lazy viewDisplay state.display
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
update msg (Interpreter state) =
    case ( msg, state.status ) of
        ( FramePassed _, _ ) ->
            runCycles 10 state
                |> Result.map updateTimers
                |> Result.map Interpreter

        ( KeyPressed key, Running ) ->
            Ok
                (Interpreter
                    { state
                        | keypad = Set.insert key state.keypad
                    }
                )

        ( KeyPressed key, WaitingForInput x ) ->
            Ok
                (Interpreter
                    { state
                        | keypad = Set.insert key state.keypad
                        , registers = Registers.set x key state.registers
                        , programCounter = state.programCounter + 2
                        , status = Running
                    }
                )

        ( KeyReleased key, _ ) ->
            Ok
                (Interpreter
                    { state
                        | keypad = Set.remove key state.keypad
                    }
                )


runCycles : Int -> State -> Result Error State
runCycles count state =
    if count <= 0 then
        Ok state

    else
        case runInstruction state (fetchInstruction state) of
            Ok newState ->
                runCycles (count - 1) newState

            Err err ->
                Err err


updateTimers : State -> State
updateTimers state =
    { state
        | delayTimer = max 0 (state.delayTimer - 1)
        , soundTimer = max 0 (state.soundTimer - 1)
    }


fetchInstruction : State -> Int
fetchInstruction state =
    let
        high =
            Memory.read (Address state.programCounter) state.memory
                |> Bitwise.shiftLeftBy 8

        low =
            Memory.read (Address (state.programCounter + 1)) state.memory
    in
    Bitwise.or high low


runInstruction : State -> Int -> Result Error State
runInstruction state instruction =
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
                        { state
                            | display = Set.empty
                            , programCounter = state.programCounter + 2
                        }

                0xEE ->
                    let
                        ( newProgramCounter, newStack ) =
                            Stack.pop state.stack
                    in
                    Ok
                        { state
                            | stack = newStack
                            , programCounter = newProgramCounter + 2
                        }

                _ ->
                    Err (InvalidInstruction instruction)

        -- 1nnn - JP nnn
        0x01 ->
            Ok { state | programCounter = nnn }

        -- 2nnn - CALL nnn
        0x02 ->
            Ok
                { state
                    | programCounter = nnn
                    , stack = Stack.push state.programCounter state.stack
                }

        -- 3xkk - SE Vx, kk
        0x03 ->
            Ok
                { state
                    | programCounter =
                        state.programCounter
                            + (if Registers.get x state.registers == kk then
                                4

                               else
                                2
                              )
                }

        -- 4xkk - SNE Vx, kk
        0x04 ->
            Ok
                { state
                    | programCounter =
                        state.programCounter
                            + (if Registers.get x state.registers /= kk then
                                4

                               else
                                2
                              )
                }

        -- 6xkk - LD Vx, kk
        0x06 ->
            Ok
                { state
                    | registers = Registers.set x kk state.registers
                    , programCounter = state.programCounter + 2
                }

        -- 7xkk - ADD Vx, kk
        0x07 ->
            Ok
                { state
                    | registers =
                        Registers.modify
                            x
                            ((+) kk >> modBy 256)
                            state.registers
                    , programCounter = state.programCounter + 2
                }

        0x08 ->
            case n of
                0x00 ->
                    Ok
                        { state
                            | registers =
                                Registers.set x
                                    (Registers.get y state.registers)
                                    state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x02 ->
                    Ok
                        { state
                            | registers =
                                Registers.set x
                                    (Bitwise.and
                                        (Registers.get x state.registers)
                                        (Registers.get y state.registers)
                                    )
                                    state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x03 ->
                    Ok
                        { state
                            | registers =
                                Registers.set x
                                    (Bitwise.xor
                                        (Registers.get x state.registers)
                                        (Registers.get y state.registers)
                                    )
                                    state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x04 ->
                    let
                        result =
                            Registers.get x state.registers
                                + Registers.get y state.registers
                    in
                    Ok
                        { state
                            | registers =
                                state.registers
                                    |> Registers.set x (modBy 256 result)
                                    |> Registers.set 0x0F
                                        (if result > 255 then
                                            1

                                         else
                                            0
                                        )
                            , programCounter = state.programCounter + 2
                        }

                0x05 ->
                    let
                        result =
                            Registers.get x state.registers
                                - Registers.get y state.registers
                    in
                    Ok
                        { state
                            | registers =
                                state.registers
                                    |> Registers.set x (modBy 256 result)
                                    |> Registers.set 0x0F
                                        (if result > 0 then
                                            1

                                         else
                                            0
                                        )
                            , programCounter = state.programCounter + 2
                        }

                _ ->
                    Err (InvalidInstruction instruction)

        -- Annn - LD I, nnn
        0x0A ->
            Ok
                { state
                    | indexRegister = nnn
                    , programCounter = state.programCounter + 2
                }

        -- Cxkk - RND Vx, kk
        0x0C ->
            let
                ( randomByte, newSeed ) =
                    Random.step randomByteGenerator state.seed
            in
            Ok
                { state
                    | seed = newSeed
                    , registers =
                        Registers.set x
                            (Bitwise.and kk randomByte)
                            state.registers
                    , programCounter = state.programCounter + 2
                }

        -- Dxyn - DRW Vx, Vy, n
        0x0D ->
            let
                rows =
                    Memory.readMany n
                        (Address state.indexRegister)
                        state.memory

                sprite =
                    spriteFromRows rows
                        |> List.map
                            (Tuple.mapBoth
                                ((+) (Registers.get x state.registers))
                                ((+) (Registers.get y state.registers))
                            )
                        |> Set.fromList

                union =
                    Set.union sprite state.display

                intersection =
                    Set.intersect sprite state.display
            in
            Ok
                { state
                    | display =
                        if Set.isEmpty intersection then
                            union

                        else
                            Set.diff union intersection
                    , registers =
                        Registers.set 0x0F
                            (if Set.isEmpty intersection then
                                0

                             else
                                1
                            )
                            state.registers
                    , programCounter = state.programCounter + 2
                }

        0x0E ->
            case kk of
                0x9E ->
                    Ok
                        { state
                            | programCounter =
                                state.programCounter
                                    + (if Set.member (Registers.get x state.registers) state.keypad then
                                        4

                                       else
                                        2
                                      )
                        }

                0xA1 ->
                    Ok
                        { state
                            | programCounter =
                                state.programCounter
                                    + (if not (Set.member (Registers.get x state.registers) state.keypad) then
                                        4

                                       else
                                        2
                                      )
                        }

                _ ->
                    Err (InvalidInstruction instruction)

        0x0F ->
            case kk of
                0x07 ->
                    Ok
                        { state
                            | registers = Registers.set x state.delayTimer state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x0A ->
                    Ok
                        { state
                            | status = WaitingForInput x
                        }

                0x15 ->
                    Ok
                        { state
                            | delayTimer = Registers.get x state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x18 ->
                    Ok
                        { state
                            | soundTimer = Registers.get x state.registers
                            , programCounter = state.programCounter + 2
                        }

                0x1E ->
                    Ok
                        { state
                            | indexRegister =
                                Registers.get x state.registers
                                    + state.indexRegister
                            , programCounter = state.programCounter + 2
                        }

                0x29 ->
                    Ok
                        { state
                            | indexRegister =
                                Memory.fontAddress
                                    + (Registers.get x state.registers * 5)
                            , programCounter = state.programCounter + 2
                        }

                0x33 ->
                    let
                        value =
                            Registers.get x state.registers
                    in
                    Ok
                        { state
                            | memory =
                                state.memory
                                    |> Memory.write (Address state.indexRegister) (value // 100)
                                    |> Memory.write (Address (state.indexRegister + 1)) (modBy 10 (value // 10))
                                    |> Memory.write (Address (state.indexRegister + 2)) (modBy 100 (modBy 10 value))
                            , programCounter = state.programCounter + 2
                        }

                0x55 ->
                    let
                        registers =
                            Registers.toList state.registers
                                |> List.take (x + 1)
                    in
                    Ok
                        { state
                            | memory =
                                Memory.writeMany (Address state.indexRegister)
                                    registers
                                    state.memory
                            , programCounter = state.programCounter + 2
                        }

                0x65 ->
                    Ok
                        { state
                            | registers =
                                Memory.readMany (x + 1)
                                    (Address state.indexRegister)
                                    state.memory
                                    |> List.Extra.indexedFoldl
                                        (\index value registers ->
                                            Registers.set index value registers
                                        )
                                        state.registers
                            , programCounter = state.programCounter + 2
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
subscriptions (Interpreter state) =
    case state.status of
        Running ->
            Sub.batch
                [ Events.onAnimationFrameDelta FramePassed
                , keyboardSubscriptions
                ]

        WaitingForInput _ ->
            keyboardSubscriptions


keyboardSubscriptions : Sub Msg
keyboardSubscriptions =
    Sub.batch
        [ Events.onKeyDown (keyDecoder KeyPressed)
        , Events.onKeyUp (keyDecoder KeyReleased)
        ]


keyDecoder : (Int -> msg) -> Decoder msg
keyDecoder toMsg =
    Decode.field "key" Decode.string
        |> Decode.andThen (keyToMsg toMsg)


keyToMsg : (Int -> msg) -> String -> Decoder msg
keyToMsg toMsg string =
    let
        succeed =
            Decode.succeed << toMsg
    in
    case String.toLower string of
        "1" ->
            succeed 0x01

        "2" ->
            succeed 0x02

        "3" ->
            succeed 0x03

        "4" ->
            succeed 0x0C

        "q" ->
            succeed 0x04

        "w" ->
            succeed 0x05

        "e" ->
            succeed 0x06

        "r" ->
            succeed 0x0D

        "a" ->
            succeed 0x07

        "s" ->
            succeed 0x08

        "d" ->
            succeed 0x09

        "f" ->
            succeed 0x0E

        "z" ->
            succeed 0x0A

        "x" ->
            succeed 0x00

        "c" ->
            succeed 0x0B

        "v" ->
            succeed 0x0F

        _ ->
            Decode.fail ("Not interested in " ++ string)
