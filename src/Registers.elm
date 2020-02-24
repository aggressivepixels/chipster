module Registers exposing (Registers, get, init, modify, set)

import Array exposing (Array)


type Registers
    = Registers (Array Int)


init : Registers
init =
    Registers (Array.repeat 16 0)


get : Int -> Registers -> Int
get index (Registers registers) =
    Array.get index registers
        |> Maybe.withDefault 0


modify : Int -> (Int -> Int) -> Registers -> Registers
modify index f (Registers registers) =
    let
        current =
            get index (Registers registers)
    in
    Registers (Array.set index (f current) registers)


set : Int -> Int -> Registers -> Registers
set index value =
    modify index (\_ -> value)
