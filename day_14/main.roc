app "advent-2023-roc-day14"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        cli.Stdout,
        array2d.Array2D.{ Array2D, Index, Shape },
        parser.Core.{ Parser, between, sepBy1, chompWhile, const, map, oneOf, buildPrimitiveParser, parsePartial, fail },
        parser.String.{ RawStr, parseStr, codeunit },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

expect part1 exampleInput == 136
expect part2 exampleInput == 64

expect part1 puzzleInput == 106517
expect part2 puzzleInput == 79723

Platform : Array2D [Rock, Wall, Empty]

main =
    Stdout.line
        """
        Part 1: \(puzzleInput |> part1 |> Num.toStr)
        Part 2: \(puzzleInput |> part2 |> Num.toStr)
        """

part1 = \input ->
    input
    |> parse
    |> tilt North
    |> totalLoad

part2 = \input ->
    input
    |> parse
    |> repeatCycleModLoop 1000000000
    |> totalLoad

repeatCycleModLoop : Platform, Nat -> Platform
repeatCycleModLoop = \platform, totalCycles ->
    { loopStart, loopEnd, cycles } = findLoop platform

    totalCyclesModLoop =
        loopStart + ((totalCycles - loopStart) % (loopEnd - loopStart))

    when List.get cycles totalCyclesModLoop is
        Ok cycledPlatform -> cycledPlatform
        Err OutOfBounds -> crash "Tried to access cycle outside of first loop"

findLoop : Platform -> { loopStart : Nat, loopEnd : Nat, cycles : List Platform }
findLoop = \platform ->
    findLoopHelp [platform] platform

findLoopHelp = \cycles, currentCycle ->
    nextCycle = cycle currentCycle

    loop = List.findFirstIndex cycles \prevCycle ->
        prevCycle == nextCycle

    when loop is
        Ok loopStart ->
            loopEnd = List.len cycles
            { loopStart, loopEnd, cycles }

        Err NotFound ->
            findLoopHelp (List.append cycles nextCycle) nextCycle

cycle : Platform -> Platform
cycle = \platform ->
    platform
    |> tilt North
    |> tilt West
    |> tilt South
    |> tilt East

tilt : Platform, [North, West, South, East] -> Platform
tilt = \platform, direction ->
    when direction is
        North -> tiltHelp platform Forwards Cols
        West -> tiltHelp platform Forwards Rows
        South -> tiltHelp platform Backwards Cols
        East -> tiltHelp platform Backwards Rows

tiltHelp = \platform, direction, orientation ->
    startState = { platform, firstOpenIndex: { x: 0, y: 0 } }

    Array2D.walk platform startState { direction, orientation } \state, elem, index ->
        startedNewRowOrCol =
            state.firstOpenIndex.x != index.x && state.firstOpenIndex.y != index.y

        firstOpenIndex =
            if startedNewRowOrCol then index else state.firstOpenIndex

        when elem is
            Rock ->
                movedRock =
                    state.platform
                    |> Array2D.set index Empty
                    |> Array2D.set firstOpenIndex Rock

                nextOpenIndex = nextIndex firstOpenIndex direction orientation

                { platform: movedRock, firstOpenIndex: nextOpenIndex }

            Wall ->
                nextOpenIndex = nextIndex index direction orientation
                { state & firstOpenIndex: nextOpenIndex }

            Empty -> { state & firstOpenIndex }
    |> .platform

nextIndex = \{ x, y }, direction, orientation ->
    when (direction, orientation) is
        (Forwards, Cols) -> { x: x + 1, y }
        (Forwards, Rows) -> { x, y: y + 1 }
        (Backwards, Cols) -> { x: Num.subWrap x 1, y }
        (Backwards, Rows) -> { x, y: Num.subWrap y 1 }

totalLoad : Platform -> Nat
totalLoad = \platform ->
    Array2D.walk platform 0 { direction: Forwards } \state, elem, index ->
        when elem is
            Rock -> state + (rockLoad platform index)
            Wall | Empty -> state

rockLoad : Platform, Index -> Nat
rockLoad = \platform, rockIndex ->
    { dimX } = Array2D.shape platform
    { x } = rockIndex
    dimX - x

parse : Str -> Platform
parse = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser =
    platformParser
    |> between optionalWhitespace optionalWhitespace

platformParser =
    table
        (oneOf [rockParser, wallParser, emptyParser])
        (const NoElemSep)
        (codeunit '\n')

rockParser = codeunit 'O' |> map \_ -> Rock

wallParser = codeunit '#' |> map \_ -> Wall

emptyParser = codeunit '.' |> map \_ -> Empty

table : Parser in a, Parser in *, Parser in * -> Parser in (Array2D a)
table = \elem, elemSep, rowSep ->
    elem
    |> sepBy1 elemSep
    |> sepBy1 rowSep
    |> andThen \rows ->
        when Array2D.fromExactLists rows is
            Ok array -> const array
            Err InconsistentRowLengths -> fail "table: rows do not have consistant lengths"

andThen : Parser input a, (a -> Parser input b) -> Parser input b
andThen = \firstParser, buildNextParser ->
    fun = \input ->
        { val: firstVal, input: rest } <- Result.try (parsePartial firstParser input)
        nextParser = buildNextParser firstVal

        parsePartial nextParser rest

    buildPrimitiveParser fun

isWhitespace : U8 -> Bool
isWhitespace = \char ->
    when char is
        ' ' -> Bool.true
        '\n' -> Bool.true
        '\t' -> Bool.true
        11 -> Bool.true # U+000B LINE TABULATION
        12 -> Bool.true # U+000C FORM FEED
        '\r' -> Bool.true
        _ -> Bool.false

optionalWhitespace : Parser (List U8) (List U8)
optionalWhitespace =
    chompWhile isWhitespace
