app "day14"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Task,
        Array2D.{ Array2D },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

# Offset makes it easier to display the results
offsetY = 300

caveDimensions = { dimX: 170, dimY: 700 - offsetY }

sandSource = { x: 0, y: 500 - offsetY }
exampleCave = initCave exampleInput
puzzleCave = initCave puzzleInput

expect exampleCave |> addMaxSand |> countSand == 24
expect puzzleCave |> addMaxSand |> countSand == 828

expect exampleCave |> addFloor |> addMaxSand |> countSand == 93
expect puzzleCave |> addFloor |> addMaxSand |> countSand == 25500

main =
    part1Cave = puzzleCave |> addMaxSand
    part1Count = part1Cave |> countSand |> Num.toStr

    part2Cave = puzzleCave |> addFloor |> addMaxSand
    part2Count = part2Cave |> countSand |> Num.toStr

    _ <- Stdout.line "Part 1: \(part1Count)" |> Task.await
    _ <- Stdout.write (displayCave part1Cave) |> Task.await
    _ <- Stdout.line "Part 2: \(part2Count)" |> Task.await
    _ <- Stdout.write (displayCave part2Cave) |> Task.await
    Task.ok {}

Cave : Array2D [Air, Rock, Sand]

Path : List Array2D.Index

addMaxSand : Cave -> Cave
addMaxSand = \cave ->
    when addSand cave sandSource is
        Ok sandyCave -> addMaxSand sandyCave
        Err _ -> cave

addSand : Cave, Array2D.Index -> Result Cave [SandFellIntoTheInfiniteAbyss, Blocked]
addSand = \cave, pos ->
    when Array2D.get cave pos is
        Err OutOfBounds -> Err SandFellIntoTheInfiniteAbyss
        Ok Sand | Ok Rock -> Err Blocked
        Ok Air ->
            addSand cave (down pos)
            |> elseIfBlocked \_ -> addSand cave (downLeft pos)
            |> elseIfBlocked \_ -> addSand cave (downRight pos)
            |> elseIfBlocked \_ -> Ok (Array2D.set cave pos Sand)

elseIfBlocked = \result, thunk ->
    when result is
        Err Blocked -> thunk {}
        _ -> result

down = \{ x, y } -> { x: x + 1, y }
downLeft = \{ x, y } -> { x: x + 1, y: y - 1 }
downRight = \{ x, y } -> { x: x + 1, y: y + 1 }

countSand : Cave -> Nat
countSand = \cave -> Array2D.countIf cave \material -> material == Sand

initCave : Str -> Cave
initCave = \input ->
    startState = Array2D.repeat Air caveDimensions
    input |> toPaths |> List.walk startState setRockPath

toPaths : Str -> List Path
toPaths = \input ->
    input
    |> Str.split "\n"
    |> List.dropLast
    |> List.map \line ->
        line
        |> Str.split " -> "
        |> List.map toPoint

toPoint : Str -> Array2D.Index
toPoint = \input ->
    when Str.split input "," is
        [strY, strX] ->
            x = strX |> Str.toNat |> orCrash "Expected a number"
            y = strY |> Str.toNat |> orCrash "Expected a number"

            { x, y: y - offsetY }

        _ -> crash "Expected a point: \(input)"

setRockPath : Cave, Path -> Cave
setRockPath = \cave, path ->
    when path is
        [point1, point2, ..] ->
            nextCave = addRockLineSegment cave point1 point2
            nextPath = List.dropFirst path
            setRockPath nextCave nextPath

        _ -> cave

addRockLineSegment : Cave, Array2D.Index, Array2D.Index -> Cave
addRockLineSegment = \cave, point1, point2 ->
    if point1.x == point2.x && point1.y != point2.y then
        # vertical line
        x = point1.x
        startY = Num.min point1.y point2.y
        endY = Num.max point1.y point2.y

        { start: At startY, end: At endY }
        |> List.range
        |> List.walk cave \state, y -> Array2D.set state { x, y } Rock
    else if point1.x != point2.x && point1.y == point2.y then
        # horizontal line
        y = point1.y
        startX = Num.min point1.x point2.x
        endX = Num.max point1.x point2.x

        { start: At startX, end: At endX }
        |> List.range
        |> List.walk cave \state, x -> Array2D.set state { x, y } Rock
    else
        crash "Diagonal lines not supported"

addFloor : Cave -> Cave
addFloor = \cave ->
    cave
    |> Array2D.findLastIndex \material -> material == Rock
    |> Result.map \{ x: floorLevel, y: _ } ->
        point1 = { x: floorLevel + 2, y: 0 }
        point2 = { x: floorLevel + 2, y: Array2D.lastCol cave }
        addRockLineSegment cave point1 point2
    |> orCrash "Error finding floor level"

displayCave : Cave -> Str
displayCave = \cave ->
    cave
    |> Array2D.mapWithIndex \material, index ->
        square =
            when material is
                Air -> "."
                Rock -> "#"
                Sand -> "o"

        if Array2D.isRowEnd cave index then
            Str.concat square "\n"
        else
            square
    |> Array2D.toList
    |> Str.joinWith ""

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
