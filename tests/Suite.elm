module Suite exposing (suite)

import Expect
import Interpreter
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Interpreter"
        [ describe "init"
            [ test "fails with a program bigger than the available memory" <|
                \_ ->
                    Interpreter.init (List.repeat (4096 - 512 + 1) 0) 0
                        |> Expect.equal Nothing
            ]
        ]
