app "day05"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, between, sepBy1, chompWhile, keep, skip, const, map, oneOf },
        parser.String.{ RawStr, parseStr, string, codeunit, digits, anyCodeunit },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

Solution : { part1 : Str, part2 : Str }

CraneSpec : [CrateMover9000, CrateMover9001]

Stack : List U8

StackId : Nat

Stacks : Dict StackId Stack

Step : { count : Nat, source : StackId, dest : StackId }

Input : { stacks : Stacks, steps : List Step }

puzzle : Solution
puzzle = puzzleInput |> parseInput |> solution

expect puzzle == { part1: "GFTNRBZPF", part2: "VRQWPDSGP" }

example : Solution
example = exampleInput |> parseInput |> solution

expect example == { part1: "CMZ", part2: "MCD" }

solution : Input -> Solution
solution = \input -> {
    part1: input |> finalState CrateMover9000 |> topCrates,
    part2: input |> finalState CrateMover9001 |> topCrates,
}

main = Stdout.line "Part 1: \(puzzle.part1)\nPart 2: \(puzzle.part2)"

topCrates : Stacks -> Str
topCrates = \stacks ->
    top = Dict.walk stacks [] \crates, _col, stack ->
        nextCrate = stack |> List.get 0 |> Result.withDefault ' '
        List.append crates nextCrate

    top |> Str.fromUtf8 |> orCrash "Error: UTF8 to String conversion issue"

finalState : Input, CraneSpec -> Stacks
finalState = \{ stacks, steps }, spec ->
    List.walk steps stacks \stacksState, step -> performStep stacksState step spec

performStep : Stacks, Step, CraneSpec -> Stacks
performStep = \stacks, step, craneSpec ->
    sourceStack = getStack stacks step.source
    destStack = getStack stacks step.dest
    { before: crates, others: newSourceStack } = List.split sourceStack step.count
    newDestStack =
        when craneSpec is
            CrateMover9000 -> List.concat (List.reverse crates) destStack
            CrateMover9001 -> List.concat crates destStack

    stacks
    |> Dict.insert step.source newSourceStack
    |> Dict.insert step.dest newDestStack

getStack : Stacks, StackId -> Stack
getStack = \stacks, stackId ->
    when Dict.get stacks stackId is
        Ok stack -> stack
        Err KeyNotFound -> crash "Error: can't find stack #\(Num.toStr stackId)"

parseInput : Str -> Input
parseInput = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input ->
            dbg input
            input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser =
    const (\stacks -> \steps -> { stacks, steps })
    |> keep stacksParser
    |> skip optionalWhitespace
    |> keep stepsParser
    |> skip optionalWhitespace

stacksParser =
    const
        (\cargoRows -> \labels ->
                cargoCols = transpose cargoRows
                crateCols = List.map cargoCols \col ->
                    List.walk col [] \acc, cargo ->
                        when cargo is
                            NoCrate -> acc
                            Crate crate -> List.append acc crate

                labels
                |> List.map2 crateCols \label, col -> (label, col)
                |> Dict.fromList
        )
    |> keep cargoRowsParser
    |> skip optionalWhitespace
    |> keep labelsParser

cargoRowsParser = sepBy1 cargoRowParser (codeunit '\n')

cargoRowParser = sepBy1 cargoParser (codeunit ' ')

cargoParser = oneOf [crateParser, noCrateParser]

crateParser =
    const (Crate)
    |> skip (codeunit '[')
    |> keep anyCodeunit
    |> skip (codeunit ']')

noCrateParser = string "   " |> map \_ -> NoCrate

labelsParser =
    digits
    |> sepBy1 optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

stepsParser =
    sepBy1 stepParser optionalWhitespace

stepParser =
    const (\count -> \source -> \dest -> { count, source, dest })
    |> skip (string "move ")
    |> keep digits
    |> skip (string " from ")
    |> keep digits
    |> skip (string " to ")
    |> keep digits

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

transpose : List (List a) -> List (List a)
transpose = \table ->
    transposeRec table []

transposeRec : List (List a), List (List a) -> List (List a)
transposeRec = \tableRest, tableAcc ->
    state = { nextTableRest: [], nextTableAccRow: [] }
    { nextTableRest, nextTableAccRow } = List.walk tableRest state \acc, row ->
        split = List.split row 1
        {
            nextTableRest: List.append acc.nextTableRest split.others,
            nextTableAccRow: List.concat acc.nextTableAccRow split.before,
        }

    if List.isEmpty nextTableAccRow then
        tableAcc
    else
        transposeRec nextTableRest (List.append tableAcc nextTableAccRow)

expect transpose [[1, 2, 3], [4, 5, 6]] == [[1, 4], [2, 5], [3, 6]]

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
