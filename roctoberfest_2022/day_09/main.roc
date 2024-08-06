app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.0/je3X2cSdUa6b24fO1SS_vGNS5MwU-a-3r1niP_7iG6k.tar.br",
}

import cli.Stdout
import cli.Task exposing [Task]
import array2d.Array2D
import "input.txt" as puzzleInput : Str
import "example.txt" as exampleInput : Str

Direction : [Right, Left, Up, Down]

Motion : (Direction, U64)

Pos : { x : I32, y : I32 }

Rope : { head : Pos, tail : List Pos }

State : { rope : Rope, visited : Set Pos }

exampleMotions = parse exampleInput
puzzleMotions = parse puzzleInput

rope2 = initState { ropeLength: 2 }
rope10 = initState { ropeLength: 10 }

expect rope2 |> moveRope exampleMotions |> countVisited == 13
expect rope10 |> moveRope exampleMotions |> countVisited == 1

expect rope2 |> moveRope puzzleMotions |> countVisited == 5710
expect rope10 |> moveRope puzzleMotions |> countVisited == 2259

main =
    part1 = moveRope rope2 puzzleMotions
    part2 = moveRope rope10 puzzleMotions

    Stdout.line! "Part1: $(part1 |> countVisited |> Num.toStr)"
    Stdout.line! (renderVisited part1)
    Stdout.line! "Part2: $(part2 |> countVisited |> Num.toStr)"
    Stdout.line! (renderVisited part2)

initState : { ropeLength : U64 } -> State
initState = \{ ropeLength } ->
    startPos = { x: 0, y: 0 }
    rope = { head: startPos, tail: List.repeat startPos (ropeLength - 1) }
    visited = Set.single startPos
    { rope, visited }

updateState : State, Rope -> State
updateState = \{ visited }, newRope ->
    last = ropeLastKnot newRope

    { rope: newRope, visited: Set.insert visited last }

countVisited = \{ visited } -> Set.len visited

ropeToList : Rope -> List Pos
ropeToList = \{ head, tail } -> List.prepend tail head

ropeLastKnot = \{ head, tail } -> tail |> List.last |> Result.withDefault head

moveRope : State, List Motion -> State
moveRope = \state, motions ->
    when motions is
        [(_, 0), .. as rest] ->
            moveRope state rest

        [(direction, steps), ..] ->
            decStepCount = (direction, steps - 1)

            state
            |> stepRope direction
            |> moveRope (List.set motions 0 decStepCount)

        [] ->
            state

stepRope : State, Direction -> State
stepRope = \state, direction ->
    newRope = state.rope |> stepHead direction |> followHead
    updateState state newRope

stepHead = \{ head: { x, y }, tail }, direction ->
    newHead =
        when direction is
            Right -> { x: x + 1, y }
            Left -> { x: x - 1, y }
            Up -> { x, y: y + 1 }
            Down -> { x, y: y - 1 }

    { head: newHead, tail }

followHead = \rope ->
    followStep = \head, tail ->
        when Num.compare head tail is
            GT -> 1
            EQ -> 0
            LT -> -1

    leader = rope.head

    when rope.tail is
        [follower, .. as rest] ->
            distX = Num.abs (leader.x - follower.x)
            distY = Num.abs (leader.y - follower.y)

            moved =
                if distX <= 1 && distY <= 1 then
                    follower
                else
                    {
                        x: follower.x + followStep leader.x follower.x,
                        y: follower.y + followStep leader.y follower.y,
                    }

            tail =
                { head: moved, tail: rest }
                |> followHead
                |> ropeToList

            { rope & tail }

        [] -> rope

renderVisited : State -> Str
renderVisited = \{ rope, visited } ->
    points = Set.toList visited
    xs = List.map points .x
    ys = List.map points .y
    minX = List.min xs |> Result.withDefault 0
    maxX = List.max xs |> Result.withDefault 0
    minY = List.min ys |> Result.withDefault 0
    maxY = List.max ys |> Result.withDefault 0
    padding = 5
    offsetX = (Num.max (minX * -1) 0) + padding
    offsetY = (Num.max (minY * -1) 0) + padding

    start = {
        row: offsetY |> Num.toU64,
        col: offsetX |> Num.toU64,
    }
    head = {
        row: rope.head.y + offsetY |> Num.toU64,
        col: rope.head.x + offsetX |> Num.toU64,
    }
    tail = {
        row: (ropeLastKnot rope).y + offsetY |> Num.toU64,
        col: (ropeLastKnot rope).x + offsetX |> Num.toU64,
    }

    {
        rows: maxY + offsetY + padding |> Num.toU64,
        cols: maxX + offsetX + padding |> Num.toU64,
    }
    |> Array2D.init \{ row, col } ->
        x = (Num.toI32 col) - offsetX
        y = (Num.toI32 row) - offsetY
        if Set.contains visited { x, y } then
            "#"
        else
            "."
    |> Array2D.set start "s"
    |> Array2D.set head "H"
    |> Array2D.set tail "T"
    |> Array2D.joinWith "" "\n"

parse : Str -> List Motion
parse = \input ->
    input
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.map \line ->
        when Str.split line " " is
            ["R", n] -> (Right, parseCount n)
            ["L", n] -> (Left, parseCount n)
            ["U", n] -> (Up, parseCount n)
            ["D", n] -> (Down, parseCount n)
            _ -> crash "Unexpected Input"

parseCount = \str -> Str.toU64 str |> orCrash "Invalid Number"

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
