module Example exposing (suite)

import Expect
import Test exposing (Test, test)


suite : Test
suite =
    test "two plus two equals four"
        (\_ -> Expect.equal 4 (2 + 2))
