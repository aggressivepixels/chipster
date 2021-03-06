port module Page.Interpreter exposing
    ( Error(..)
    , Interpreter
    , InvalidProgram
    , Msg
    , make
    , subscriptions
    , update
    , view
    )

import Bitwise
import Browser.Events as BE
import Html as H
import Json.Decode as JD
import List.Extra
import Page.Interpreter.Memory as Memory exposing (Address(..), Memory)
import Page.Interpreter.Stack as Stack exposing (Stack)
import Page.Interpreter.V as V exposing (V)
import Random exposing (Generator, Seed)
import Set exposing (Set)
import Svg as S
import Svg.Attributes as SA
import Svg.Keyed as SK
import Svg.Lazy as SL
import Task exposing (Task)
import Time


port beep : () -> Cmd msg


type Interpreter
    = Interpreter State


type alias State =
    { memory : Memory
    , pc : Int
    , i : Int
    , display : Set Pixel
    , seed : Seed
    , v : V
    , stack : Stack
    , status : Status
    , keypad : Set Int
    , dt : Int
    , st : Int
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


type InvalidProgram
    = InvalidProgram


make : List Int -> Task InvalidProgram Interpreter
make program =
    Time.now
        |> Task.andThen
            (\seed ->
                case init program (Time.posixToMillis seed) of
                    Just interpreter ->
                        Task.succeed interpreter

                    Nothing ->
                        Task.fail InvalidProgram
            )



-- INIT


init : List Int -> Int -> Maybe Interpreter
init program seed =
    Memory.init program
        |> Maybe.map
            (\memory ->
                Interpreter
                    { memory = memory
                    , pc = 512
                    , i = 0
                    , v = V.init
                    , display = Set.empty
                    , seed = Random.initialSeed seed
                    , stack = Stack.init
                    , status = Running
                    , keypad = Set.empty
                    , dt = 0
                    , st = 0
                    }
            )



-- VIEW


view : Interpreter -> H.Html msg
view (Interpreter state) =
    S.svg [ SA.width "640", SA.height "320", SA.viewBox "0 0 64 32" ]
        [ viewBackground
        , SL.lazy viewDisplay state.display
        ]


viewBackground : S.Svg msg
viewBackground =
    S.rect
        [ SA.x "0"
        , SA.y "0"
        , SA.width "64"
        , SA.height "32"
        , SA.fill "black"
        ]
        []


viewDisplay : Set Pixel -> S.Svg msg
viewDisplay =
    Set.toList >> List.map viewKeyedPixel >> SK.node "g" []


viewKeyedPixel : Pixel -> ( String, S.Svg msg )
viewKeyedPixel ( x, y ) =
    ( String.fromInt x ++ "," ++ String.fromInt y
    , SL.lazy viewPixel ( x, y )
    )


viewPixel : Pixel -> S.Svg msg
viewPixel ( x, y ) =
    S.rect
        [ SA.x (String.fromInt x)
        , SA.y (String.fromInt y)
        , SA.width "1"
        , SA.height "1"
        , SA.fill "white"
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
                        , if state.st == 1 then
                            beep ()

                          else
                            Cmd.none
                        )
                    )

        ( KeyPressed key, Running ) ->
            Ok
                ( Interpreter { state | keypad = Set.insert key state.keypad }
                , Cmd.none
                )

        ( KeyPressed key, WaitingForInput x ) ->
            Ok
                ( Interpreter
                    { state
                        | keypad = Set.insert key state.keypad
                        , v = V.set x key state.v
                        , status = Running
                    }
                , Cmd.none
                )

        ( KeyReleased key, _ ) ->
            Ok
                ( Interpreter { state | keypad = Set.remove key state.keypad }
                , Cmd.none
                )


runCycles : Int -> State -> Result Error State
runCycles count state =
    if count <= 0 then
        Ok state

    else
        case runInstruction (nextInstruction state) (fetchInstruction state) of
            Ok newState ->
                case newState.status of
                    WaitingForInput _ ->
                        Ok newState

                    _ ->
                        runCycles (count - 1) newState

            Err err ->
                Err err


updateTimers : State -> State
updateTimers state =
    { state
        | dt = max 0 (state.dt - 1)
        , st = max 0 (state.st - 1)
    }


fetchInstruction : State -> Int
fetchInstruction state =
    let
        high =
            Memory.read (Address state.pc) state.memory
                |> Bitwise.shiftLeftBy 8

        low =
            Memory.read (Address (state.pc + 1)) state.memory
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

        vx =
            V.get x state.v

        vy =
            V.get y state.v
    in
    case ( op, y, n ) of
        -- 00E0 - CLS
        ( 0x00, 0x0E, 0x00 ) ->
            Ok (clearDisplay state)

        -- 00EE - RET
        ( 0x00, 0x0E, 0x0E ) ->
            Ok (popStack state)

        -- 1nnn - JP nnn
        ( 0x01, _, _ ) ->
            Ok (jumpTo nnn state)

        -- 2nnn - CALL nnn
        ( 0x02, _, _ ) ->
            pushStack state
                |> jumpTo nnn
                |> Ok

        -- 3xkk - SE Vx, kk
        ( 0x03, _, _ ) ->
            Ok (nextInstructionIf (vx == kk) state)

        -- 4xkk - SNE Vx, kk
        ( 0x04, _, _ ) ->
            Ok (nextInstructionIf (vx /= kk) state)

        -- 5xy0 - SE Vx, Vy
        ( 0x05, _, _ ) ->
            Ok (nextInstructionIf (vx == vy) state)

        -- 6xkk - LD Vx, kk
        ( 0x06, _, _ ) ->
            Ok (setV kk x state)

        -- 7xkk - ADD Vx, kk
        ( 0x07, _, _ ) ->
            Ok (modifyV ((+) kk >> modBy 256) x state)

        -- 8xy0 - LD Vx, Vy
        ( 0x08, _, 0x00 ) ->
            Ok (setV vy x state)

        -- 8xy1 - OR Vx, Vy
        ( 0x08, _, 0x01 ) ->
            Ok (setV (Bitwise.or vx vy) x state)

        -- 8xy2 - AND Vx, Vy
        ( 0x08, _, 0x02 ) ->
            Ok (setV (Bitwise.and vx vy) x state)

        -- 8xy3 - XOR Vx, Vy
        ( 0x08, _, 0x03 ) ->
            Ok (setV (Bitwise.xor vx vy) x state)

        -- 8xy4 - ADD Vx, Vy
        ( 0x08, _, 0x04 ) ->
            let
                result =
                    vx + vy
            in
            setV (modBy 256 result) x state
                |> updateVf (result > 255)
                |> Ok

        -- 8xy5 - SUB Vx, Vy
        ( 0x08, _, 0x05 ) ->
            let
                result =
                    vx - vy
            in
            setV (modBy 256 result) x state
                |> updateVf (result > 0)
                |> Ok

        -- 8xy6 - SHR Vx
        ( 0x08, _, 0x06 ) ->
            modifyV (Bitwise.shiftRightZfBy 1) x state
                |> setV (Bitwise.and 1 vx) 0x0F
                |> Ok

        -- 8xy7 - SUBN Vx, Vy
        ( 0x08, _, 0x07 ) ->
            let
                result =
                    vy - vx
            in
            setV (modBy 256 result) x state
                |> updateVf (result > 0)
                |> Ok

        -- 8xyE - SHL Vx
        ( 0x08, _, 0x0E ) ->
            modifyV (Bitwise.shiftLeftBy 1 >> modBy 256) x state
                |> setV (Bitwise.shiftRightZfBy 7 vx) 0x0F
                |> Ok

        -- 9xy0 - SNE Vx, Vy
        ( 0x09, _, _ ) ->
            Ok (nextInstructionIf (vx /= vy) state)

        -- Annn - LD I, nnn
        ( 0x0A, _, _ ) ->
            Ok (setI nnn state)

        -- Bnnn - JP V0, nnn
        ( 0x0B, _, _ ) ->
            Ok (jumpTo (V.get 0 state.v + nnn) state)

        -- Cxkk - RND Vx, kk
        ( 0x0C, _, _ ) ->
            let
                ( randomByte, newState ) =
                    stepSeed state
            in
            Ok (setV (Bitwise.and kk randomByte) x newState)

        -- Dxyn - DRW Vx, Vy, n
        ( 0x0D, _, _ ) ->
            let
                rows =
                    Memory.readMany n (Address state.i) state.memory

                sprite =
                    spriteFromRows rows
                        |> List.map (Tuple.mapBoth ((+) vx) ((+) vy))
                        |> Set.fromList

                union =
                    Set.union sprite state.display

                intersection =
                    Set.intersect sprite state.display

                ( newDisplay, noPixelsErased ) =
                    if Set.isEmpty intersection then
                        ( union, True )

                    else
                        ( Set.diff union intersection, False )
            in
            { state | display = newDisplay }
                |> updateVf (not noPixelsErased)
                |> Ok

        -- Ex9E - SKP Vx
        ( 0x0E, 0x09, 0x0E ) ->
            Ok (nextInstructionIf (Set.member vx state.keypad) state)

        -- ExA1 - SKNP Vx
        ( 0x0E, 0x0A, 0x01 ) ->
            Ok (nextInstructionIf (not (Set.member vx state.keypad)) state)

        -- Fx07 - LD Vx, DT
        ( 0x0F, 0x00, 0x07 ) ->
            Ok (setV state.dt x state)

        -- Fx0A - LD Vx, K
        ( 0x0F, 0x00, 0x0A ) ->
            Ok { state | status = WaitingForInput x }

        -- Fx15 - LD DT, Vx
        ( 0x0F, 0x01, 0x05 ) ->
            Ok { state | dt = vx }

        -- Fx18 - LD ST, Vx
        ( 0x0F, 0x01, 0x08 ) ->
            Ok { state | st = vx }

        -- Fx1E - ADD I, Vx
        ( 0x0F, 0x01, 0x0E ) ->
            Ok (modifyI ((+) vx >> modBy (2 ^ 16)) state)

        -- Fx29 - LD F, Vx
        ( 0x0F, 0x02, 0x09 ) ->
            Ok (setI (Memory.fontAddress + (5 * vx)) state)

        -- Fx33 - LD B, Vx
        ( 0x0F, 0x03, 0x03 ) ->
            let
                write addr =
                    Memory.write (Address (state.i + addr))
            in
            Ok
                { state
                    | memory =
                        state.memory
                            |> write 0 (vx // 100)
                            |> write 1 (modBy 10 (vx // 10))
                            |> write 2 (modBy 100 (modBy 10 vx))
                }

        -- Fx55 - LD [I], Vx
        ( 0x0F, 0x05, 0x05 ) ->
            let
                v =
                    V.toList state.v
                        |> List.take (x + 1)
            in
            Ok
                { state
                    | memory =
                        Memory.writeMany (Address state.i) v state.memory
                }

        -- Fx65 - LD Vx, [I]
        ( 0x0F, 0x06, 0x05 ) ->
            let
                memoryV =
                    Memory.readMany (x + 1) (Address state.i) state.memory
            in
            Ok
                { state
                    | v = List.Extra.indexedFoldl V.set state.v memoryV
                }

        _ ->
            Err (InvalidInstruction instruction)


nextInstructionIf : Bool -> State -> State
nextInstructionIf cond state =
    if cond then
        nextInstruction state

    else
        state


clearDisplay : State -> State
clearDisplay state =
    { state | display = Set.empty }


nextInstruction : State -> State
nextInstruction state =
    { state | pc = state.pc + 2 }


popStack : State -> State
popStack state =
    let
        ( newPc, newStack ) =
            Stack.pop state.stack
    in
    { state
        | stack = newStack
        , pc = newPc + 2
    }


jumpTo : Int -> State -> State
jumpTo addr state =
    { state | pc = addr }


pushStack : State -> State
pushStack state =
    { state | stack = Stack.push (state.pc - 2) state.stack }


modifyV : (Int -> Int) -> Int -> State -> State
modifyV f target state =
    { state | v = V.modify target f state.v }


setV : Int -> Int -> State -> State
setV value =
    modifyV (\_ -> value)


updateVf : Bool -> State -> State
updateVf cond =
    setV
        (if cond then
            1

         else
            0
        )
        0x0F


modifyI : (Int -> Int) -> State -> State
modifyI f state =
    { state | i = f state.i }


setI : Int -> State -> State
setI value =
    modifyI (\_ -> value)


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
getBitsHelp soFar count int =
    if count <= 0 then
        List.reverse soFar

    else
        let
            mask =
                Bitwise.shiftLeftBy (count - 1) 1

            isOn =
                Bitwise.and mask int /= 0
        in
        getBitsHelp (isOn :: soFar) (count - 1) int


randomByteGenerator : Generator Int
randomByteGenerator =
    Random.int 0 255



-- SUBSCRIPTIONS


subscriptions : Interpreter -> Sub Msg
subscriptions (Interpreter state) =
    case state.status of
        Running ->
            Sub.batch
                [ BE.onAnimationFrameDelta FramePassed
                , keyboardSubscriptions
                ]

        WaitingForInput _ ->
            keyboardSubscriptions


keyboardSubscriptions : Sub Msg
keyboardSubscriptions =
    Sub.batch
        [ BE.onKeyDown (keyDecoder KeyPressed)
        , BE.onKeyUp (keyDecoder KeyReleased)
        ]


keyDecoder : (Int -> msg) -> JD.Decoder msg
keyDecoder toMsg =
    JD.field "key" JD.string
        |> JD.andThen (keyToMsg toMsg)


keyToMsg : (Int -> msg) -> String -> JD.Decoder msg
keyToMsg toMsg string =
    let
        succeed =
            JD.succeed << toMsg
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
            JD.fail ("Not interested in " ++ string)
