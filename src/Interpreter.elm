port module Interpreter exposing
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
import Interpreter.Memory as Memory exposing (Address(..), Memory)
import Interpreter.Registers as Registers exposing (Registers)
import Interpreter.Stack as Stack exposing (Stack)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed as Keyed
import Svg.Lazy exposing (lazy)


port beep : () -> Cmd msg


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


update : Msg -> Interpreter -> Result Error ( Interpreter, Cmd Msg )
update msg (Interpreter state) =
    case ( msg, state.status ) of
        ( FramePassed _, _ ) ->
            runCycles 10 state
                |> Result.map updateTimers
                |> Result.map
                    (\newState ->
                        ( Interpreter newState
                        , if state.soundTimer == 1 then
                            beep ()

                          else
                            Cmd.none
                        )
                    )

        ( KeyPressed key, Running ) ->
            Ok
                ( Interpreter
                    { state
                        | keypad = Set.insert key state.keypad
                    }
                , Cmd.none
                )

        ( KeyPressed key, WaitingForInput x ) ->
            Ok
                ( Interpreter
                    { state
                        | keypad = Set.insert key state.keypad
                        , registers = Registers.set x key state.registers
                        , programCounter = state.programCounter + 2
                        , status = Running
                    }
                , Cmd.none
                )

        ( KeyReleased key, _ ) ->
            Ok
                ( Interpreter
                    { state
                        | keypad = Set.remove key state.keypad
                    }
                , Cmd.none
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

        xValue =
            Registers.get x state.registers

        yValue =
            Registers.get y state.registers
    in
    case op of
        0x00 ->
            case kk of
                0xE0 ->
                    clearDisplay state
                        |> nextInstruction
                        |> Ok

                0xEE ->
                    popStack state
                        |> nextInstruction
                        |> Ok

                _ ->
                    Err (InvalidInstruction instruction)

        -- 1nnn - JP nnn
        0x01 ->
            Ok (jumpTo nnn state)

        -- 2nnn - CALL nnn
        0x02 ->
            pushStack state
                |> jumpTo nnn
                |> Ok

        -- 3xkk - SE Vx, kk
        0x03 ->
            nextInstruction state
                |> (if xValue == kk then
                        nextInstruction

                    else
                        identity
                   )
                |> Ok

        -- 4xkk - SNE Vx, kk
        0x04 ->
            nextInstruction state
                |> (if xValue /= kk then
                        nextInstruction

                    else
                        identity
                   )
                |> Ok

        -- 6xkk - LD Vx, kk
        0x06 ->
            setRegister kk x state
                |> nextInstruction
                |> Ok

        -- 7xkk - ADD Vx, kk
        0x07 ->
            modifyRegister ((+) kk >> modBy 256) x state
                |> nextInstruction
                |> Ok

        0x08 ->
            case n of
                0x00 ->
                    setRegister yValue x state
                        |> nextInstruction
                        |> Ok

                0x01 ->
                    setRegister (Bitwise.or xValue yValue) x state
                        |> nextInstruction
                        |> Ok

                0x02 ->
                    setRegister (Bitwise.and xValue yValue) x state
                        |> nextInstruction
                        |> Ok

                0x03 ->
                    setRegister (Bitwise.xor xValue yValue) x state
                        |> nextInstruction
                        |> Ok

                0x04 ->
                    let
                        result =
                            xValue + yValue
                    in
                    setRegister (modBy 256 result) x state
                        |> setRegister
                            (if result > 255 then
                                1

                             else
                                0
                            )
                            0x0F
                        |> nextInstruction
                        |> Ok

                0x05 ->
                    let
                        result =
                            xValue - yValue
                    in
                    setRegister (modBy 256 result) x state
                        |> setRegister
                            (if result > 0 then
                                1

                             else
                                0
                            )
                            0x0F
                        |> nextInstruction
                        |> Ok

                0x06 ->
                    modifyRegister (Bitwise.shiftRightZfBy 1) x state
                        |> nextInstruction
                        |> Ok

                0x07 ->
                    let
                        result =
                            yValue - xValue
                    in
                    setRegister (modBy 256 result) x state
                        |> setRegister
                            (if result > 0 then
                                1

                             else
                                0
                            )
                            0x0F
                        |> nextInstruction
                        |> Ok

                0x0E ->
                    modifyRegister (Bitwise.shiftLeftBy 1) x state
                        |> nextInstruction
                        |> Ok

                _ ->
                    Err (InvalidInstruction instruction)

        -- 9xy0 - SNE Vx, Vy
        0x09 ->
            nextInstruction state
                |> (if xValue /= yValue then
                        nextInstruction

                    else
                        identity
                   )
                |> Ok

        -- Annn - LD I, nnn
        0x0A ->
            setIndexRegister nnn state
                |> nextInstruction
                |> Ok

        -- Cxkk - RND Vx, kk
        0x0C ->
            let
                ( randomByte, newState ) =
                    stepSeed state
            in
            setRegister (Bitwise.and kk randomByte) x newState
                |> nextInstruction
                |> Ok

        -- Dxyn - DRW Vx, Vy, n
        0x0D ->
            let
                rows =
                    Memory.readMany n
                        (Address state.indexRegister)
                        state.memory

                sprite =
                    spriteFromRows rows
                        |> List.map (Tuple.mapBoth ((+) xValue) ((+) yValue))
                        |> Set.fromList

                union =
                    Set.union sprite state.display

                intersection =
                    Set.intersect sprite state.display
            in
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
            }
                |> nextInstruction
                |> Ok

        0x0E ->
            case kk of
                0x9E ->
                    nextInstruction state
                        |> (if Set.member xValue state.keypad then
                                nextInstruction

                            else
                                identity
                           )
                        |> Ok

                0xA1 ->
                    nextInstruction state
                        |> (if not (Set.member xValue state.keypad) then
                                nextInstruction

                            else
                                identity
                           )
                        |> Ok

                _ ->
                    Err (InvalidInstruction instruction)

        0x0F ->
            case kk of
                0x07 ->
                    setRegister state.delayTimer x state
                        |> nextInstruction
                        |> Ok

                0x0A ->
                    Ok { state | status = WaitingForInput x }

                0x15 ->
                    { state | delayTimer = Registers.get x state.registers }
                        |> nextInstruction
                        |> Ok

                0x18 ->
                    { state | soundTimer = Registers.get x state.registers }
                        |> nextInstruction
                        |> Ok

                0x1E ->
                    modifyIndexRegister ((+) xValue) state
                        |> nextInstruction
                        |> Ok

                0x29 ->
                    setIndexRegister (Memory.fontAddress + (5 * xValue)) state
                        |> nextInstruction
                        |> Ok

                0x33 ->
                    let
                        write addr =
                            Memory.write (Address (state.indexRegister + addr))
                    in
                    { state
                        | memory =
                            state.memory
                                |> write 0 (xValue // 100)
                                |> write 1 (modBy 10 (xValue // 10))
                                |> write 2 (modBy 100 (modBy 10 xValue))
                    }
                        |> nextInstruction
                        |> Ok

                0x55 ->
                    let
                        registers =
                            Registers.toList state.registers
                                |> List.take (x + 1)
                    in
                    { state
                        | memory =
                            Memory.writeMany (Address state.indexRegister)
                                registers
                                state.memory
                    }
                        |> nextInstruction
                        |> Ok

                0x65 ->
                    let
                        memoryRegisters =
                            Memory.readMany (x + 1)
                                (Address state.indexRegister)
                                state.memory
                    in
                    { state
                        | registers =
                            List.Extra.indexedFoldl Registers.set
                                state.registers
                                memoryRegisters
                    }
                        |> nextInstruction
                        |> Ok

                _ ->
                    Err (InvalidInstruction instruction)

        _ ->
            Err (InvalidInstruction instruction)


clearDisplay : State -> State
clearDisplay state =
    { state | display = Set.empty }


nextInstruction : State -> State
nextInstruction state =
    { state | programCounter = state.programCounter + 2 }


popStack : State -> State
popStack state =
    let
        ( newProgramCounter, newStack ) =
            Stack.pop state.stack
    in
    { state
        | stack = newStack
        , programCounter = newProgramCounter
    }


jumpTo : Int -> State -> State
jumpTo addr state =
    { state | programCounter = addr }


pushStack : State -> State
pushStack state =
    { state | stack = Stack.push state.programCounter state.stack }


modifyRegister : (Int -> Int) -> Int -> State -> State
modifyRegister f target state =
    { state | registers = Registers.modify target f state.registers }


setRegister : Int -> Int -> State -> State
setRegister value =
    modifyRegister (\_ -> value)


modifyIndexRegister : (Int -> Int) -> State -> State
modifyIndexRegister f state =
    { state | indexRegister = f state.indexRegister }


setIndexRegister : Int -> State -> State
setIndexRegister value =
    modifyIndexRegister (\_ -> value)


stepSeed : State -> ( Int, State )
stepSeed state =
    let
        ( randomByte, newSeed ) =
            Random.step randomByteGenerator state.seed
    in
    ( randomByte, { state | seed = newSeed } )


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
