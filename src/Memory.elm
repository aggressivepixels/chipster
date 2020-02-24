module Memory exposing
    ( Address(..)
    , Memory
    , init
    , read
    , readMany
    , write
    )

import Dict exposing (Dict)


type Memory
    = Memory (Dict Int Int)


type Address
    = Address Int


init : List Int -> Maybe Memory
init program =
    if List.length program < 4096 - 512 then
        let
            indexedBytes =
                List.indexedMap (\index byte -> ( 512 + index, byte )) program
        in
        Just (Memory (Dict.fromList indexedBytes))

    else
        Nothing


read : Address -> Memory -> Int
read (Address address) (Memory memory) =
    if address >= 0 && address < 4096 then
        Maybe.withDefault 0 (Dict.get address memory)

    else
        0


readMany : Int -> Address -> Memory -> List Int
readMany count (Address address) (Memory memory) =
    readManyHelp [] count address memory


readManyHelp : List Int -> Int -> Int -> Dict Int Int -> List Int
readManyHelp soFar count address memory =
    if count < 1 then
        soFar

    else
        let
            byte =
                read (Address (address + count - 1)) (Memory memory)
        in
        readManyHelp (byte :: soFar) (count - 1) address memory


write : Address -> Int -> Memory -> Memory
write (Address address) byte (Memory memory) =
    if address >= 512 && address < 4096 then
        Memory (Dict.insert address byte memory)

    else
        Memory memory
