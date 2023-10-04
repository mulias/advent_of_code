app "day04"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, between, sepBy, map, chompUntil, chompWhile, keep, skip, const, oneOrMore },
        parser.String.{ RawStr, parseStr, codeunit, digit },
        Range.{ Range },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

main =
    part1 = "foo"
    part2 = "bar"
    Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

AssignmentPair : (Range Nat, Range Nat)

exampleAssignmentPairs = parseInput exampleInput

expect fullOverlapCount exampleAssignmentPairs == 2

fullOverlapCount : List AssignmentPair -> Nat
fullOverlapCount = \assignmentPairs ->
    (elf1, elf2) <- List.countIf assignmentPairs
    Range.isContained elf1 elf2 || Range.isContained elf2 elf1

parseInput : Str -> List AssignmentPair
parseInput = \input ->
    when parseStr inputParser input is
        Ok pairs -> pairs
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser =
    assignmentPairParser
    |> sepBy optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

assignmentPairParser =
    const (\elf1 -> \elf2 -> (elf1, elf2))
    |> keep rangeParser
    |> skip (codeunit ',')
    |> keep rangeParser

rangeParser : Parser RawStr (Range Nat)
rangeParser =
    const (\first -> \last -> Range.new first last)
    |> keep digits
    |> skip (codeunit '-')
    |> keep digits

digits : Parser RawStr Nat
digits =
    oneOrMore digit
    |> map \ds -> List.walk ds 0 (\sum, d -> sum * 10 + d)

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

