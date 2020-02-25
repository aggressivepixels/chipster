module Interpreter.V exposing
    ( V
    , get
    , init
    , modify
    , set
    , toList
    )

import Array exposing (Array)


type V
    = V (Array Int)


init : V
init =
    V (Array.repeat 16 0)


get : Int -> V -> Int
get index (V registers) =
    Array.get index registers
        |> Maybe.withDefault 0


modify : Int -> (Int -> Int) -> V -> V
modify index f (V registers) =
    let
        current =
            get index (V registers)
    in
    V (Array.set index (f current) registers)


set : Int -> Int -> V -> V
set index value =
    modify index (\_ -> value)


toList : V -> List Int
toList (V registers) =
    Array.toList registers
