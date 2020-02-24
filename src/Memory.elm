module Memory exposing
    ( Address(..)
    , Memory
    , fontAddress
    , init
    , read
    , readMany
    , write
    , writeMany
    )

import Dict exposing (Dict)


type Memory
    = Memory (Dict Int Int)


type Address
    = Address Int


fontAddress : Int
fontAddress =
    0x80


font : Dict Int Int
font =
    Dict.fromList
        (List.indexedMap (\index value -> ( index + fontAddress, value ))
            [ 0xF0
            , 0x90
            , 0x90
            , 0x90
            , 0xF0
            , 0x20
            , 0x60
            , 0x20
            , 0x20
            , 0x70
            , 0xF0
            , 0x10
            , 0xF0
            , 0x80
            , 0xF0
            , 0xF0
            , 0x10
            , 0xF0
            , 0x10
            , 0xF0
            , 0x90
            , 0x90
            , 0xF0
            , 0x10
            , 0x10
            , 0xF0
            , 0x80
            , 0xF0
            , 0x10
            , 0xF0
            , 0xF0
            , 0x80
            , 0xF0
            , 0x90
            , 0xF0
            , 0xF0
            , 0x10
            , 0x20
            , 0x40
            , 0x40
            , 0xF0
            , 0x90
            , 0xF0
            , 0x90
            , 0xF0
            , 0xF0
            , 0x90
            , 0xF0
            , 0x10
            , 0xF0
            , 0xF0
            , 0x90
            , 0xF0
            , 0x90
            , 0x90
            , 0xE0
            , 0x90
            , 0xE0
            , 0x90
            , 0xE0
            , 0xF0
            , 0x80
            , 0x80
            , 0x80
            , 0xF0
            , 0xE0
            , 0x90
            , 0x90
            , 0x90
            , 0xE0
            , 0xF0
            , 0x80
            , 0xF0
            , 0x80
            , 0xF0
            , 0xF0
            , 0x80
            , 0xF0
            , 0x80
            , 0x80
            ]
        )


init : List Int -> Maybe Memory
init program =
    if List.length program < 4096 - 512 then
        let
            indexedBytes =
                List.indexedMap (\index byte -> ( 512 + index, byte )) program
        in
        Just (Memory (Dict.union font (Dict.fromList indexedBytes)))

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


writeMany : Address -> List Int -> Memory -> Memory
writeMany (Address address) bytes memory =
    writeManyHelp address (List.reverse bytes) memory


writeManyHelp : Int -> List Int -> Memory -> Memory
writeManyHelp address bytes memory =
    case bytes of
        [] ->
            memory

        x :: xs ->
            writeManyHelp address
                xs
                (write (Address (address + List.length xs)) x memory)
