app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import cli.Stdout
import parser.Core exposing [Parser, between, sepBy, chompWhile, keep, skip, const]
import parser.String exposing [Utf8, parseStr, codeunit, digits]
import Range exposing [Range]
import "input.txt" as puzzleInput : Str
import "example.txt" as exampleInput : Str

main =
    part1 = completeOverlapCount puzzleAssignmentPairs |> Num.toStr
    part2 = anyOverlapCount puzzleAssignmentPairs |> Num.toStr
    Stdout.line "Part 1: $(part1)\nPart 2: $(part2)"

AssignmentPair : (Range, Range)

exampleAssignmentPairs = parseInput exampleInput
puzzleAssignmentPairs = parseInput puzzleInput

expect completeOverlapCount exampleAssignmentPairs == 2
expect completeOverlapCount puzzleAssignmentPairs == 500

expect anyOverlapCount exampleAssignmentPairs == 4
expect anyOverlapCount puzzleAssignmentPairs == 815

completeOverlapCount : List AssignmentPair -> U64
completeOverlapCount = \assignmentPairs ->
    List.countIf assignmentPairs \(elf1, elf2) ->
        Range.isContained elf1 elf2 || Range.isContained elf2 elf1

anyOverlapCount : List AssignmentPair -> U64
anyOverlapCount = \assignmentPairs ->
    List.countIf assignmentPairs \(elf1, elf2) ->
        Range.intersection elf1 elf2 != Disjoint

parseInput : Str -> List AssignmentPair
parseInput = \input ->
    when parseStr inputParser input is
        Ok pairs -> pairs
        Err (ParsingFailure msg) -> crash "parsing failure '$(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '$(leftover)'"

inputParser =
    assignmentPairParser
    |> sepBy optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

assignmentPairParser =
    const (\elf1 -> \elf2 -> (elf1, elf2))
    |> keep rangeParser
    |> skip (codeunit ',')
    |> keep rangeParser

rangeParser : Parser Utf8 Range
rangeParser =
    const (\first -> \last -> Range.new first last)
    |> keep digits
    |> skip (codeunit '-')
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
