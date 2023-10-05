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

Crate : U8

Stack : List Crate

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
        nextCrate = stack |> List.get 0 |> orCrash "Unexpected empty stack"
        List.append crates nextCrate

    top |> Str.fromUtf8 |> orCrash "Error: UTF8 to String conversion issue"

finalState : Input, CraneSpec -> Stacks
finalState = \{ stacks, steps }, spec ->
    List.walk steps stacks \stacksState, step -> performStep stacksState step spec

performStep : Stacks, Step, CraneSpec -> Stacks
performStep = \stacks, step, craneSpec ->
    sourceStack = getStack stacks step.source
    destStack = getStack stacks step.dest

    { before: movedCrates, others: newSourceStack } = List.split sourceStack step.count

    newDestStack =
        when craneSpec is
            CrateMover9000 -> List.concat (List.reverse movedCrates) destStack
            CrateMover9001 -> List.concat movedCrates destStack

    stacks
    |> Dict.insert step.source newSourceStack
    |> Dict.insert step.dest newDestStack

getStack : Stacks, StackId -> Stack
getStack = \stacks, stackId ->
    stacks |> Dict.get stackId |> orCrash "Error: can't find stack #\(Num.toStr stackId)"

parseInput : Str -> Input
parseInput = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser : Parser RawStr Input
inputParser =
    const (\stacks -> \steps -> { stacks, steps })
    |> keep stacksParser
    |> skip optionalWhitespace
    |> keep stepsParser
    |> skip optionalWhitespace

stacksParser : Parser RawStr Stacks
stacksParser =
    const
        (\crateCols -> \labels ->
                labels
                |> List.map2 crateCols \label, col -> (label, col)
                |> Dict.fromList
        )
    |> keep crateColsParser
    |> skip optionalWhitespace
    |> keep labelsParser

# parse rows of `Cargo` and `NoCargo` values, transpose the rows to get the
# columns as seperate lists, then remove the `NoCargo` values since they should
# now be at the end of each col list
crateColsParser : Parser RawStr (List Stack)
crateColsParser =
    cargoRows <- map cargoRowsParser
    col <- List.map (transpose cargoRows)
    acc, cargo <- List.walk col []
    when cargo is
        NoCargo -> acc
        Cargo crate -> List.append acc crate

cargoRowsParser : Parser RawStr (List (List [Cargo Crate, NoCargo]))
cargoRowsParser = sepBy1 cargoRowParser (codeunit '\n')

cargoRowParser : Parser RawStr (List [Cargo Crate, NoCargo])
cargoRowParser = sepBy1 cargoParser (codeunit ' ')

cargoParser : Parser RawStr [Cargo Crate, NoCargo]
cargoParser = oneOf [crateParser, noCargoParser]

crateParser =
    const (Cargo)
    |> skip (codeunit '[')
    |> keep anyCodeunit
    |> skip (codeunit ']')

noCargoParser = string "   " |> map \_ -> NoCargo

labelsParser : Parser RawStr (List Nat)
labelsParser =
    digits
    |> sepBy1 optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

stepsParser : Parser RawStr (List Step)
stepsParser =
    sepBy1 stepParser optionalWhitespace

stepParser : Parser RawStr Step
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

optionalWhitespace : Parser RawStr RawStr
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
