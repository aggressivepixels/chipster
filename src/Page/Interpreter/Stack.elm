module Page.Interpreter.Stack exposing (Stack, init, pop, push)


type Stack
    = Stack (List Int)


init : Stack
init =
    Stack []


push : Int -> Stack -> Stack
push value (Stack stack) =
    Stack (value :: stack)


pop : Stack -> ( Int, Stack )
pop (Stack stack) =
    case stack of
        [] ->
            ( 0, Stack [] )

        first :: rest ->
            ( first, Stack rest )
