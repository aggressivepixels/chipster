module Hex exposing (toHexString)


toHexString : Int -> String
toHexString =
    toHexStringHelp [] >> String.fromList


toHexStringHelp : List Char -> Int -> List Char
toHexStringHelp soFar int =
    if int > 0 then
        toHexStringHelp (unsafeHexChar (modBy 16 int) :: soFar) (int // 16)

    else
        soFar


unsafeHexChar : Int -> Char
unsafeHexChar int =
    case int of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'A'

        11 ->
            'B'

        12 ->
            'C'

        13 ->
            'D'

        14 ->
            'E'

        15 ->
            'F'

        _ ->
            unsafeHexChar int
