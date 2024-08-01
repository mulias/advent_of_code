app "day01"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Task,
        parser.Core.{ Parser, between, sepBy1, map, chompWhile, flatten, oneOf, oneOrMore, skip, const, keep, buildPrimitiveParser },
        parser.String.{ parseStr, codeunit, digit, codeunitSatisfies, anyCodeunit, RawStr },
        "input.txt" as puzzleInput : Str,
    ]
    provides [main] to cli

expect parse puzzleInput == Ok { part1: 54708, part2: 54087 }

main =
    when parse puzzleInput is
        Ok { part1, part2 } ->
            {} <- Stdout.line "Part 1: \(Num.toStr part1)" |> Task.await
            {} <- Stdout.line "Part 2: \(Num.toStr part2)" |> Task.await
            Task.ok {}

        Err (ParsingFailure msg) -> Stdout.line "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> Stdout.line "parsing incomplete '\(leftover)'"

parse = \input -> parseStr calibrationTotals input

calibrationTotals =
    calibrationLine
    |> flatten
    |> sepBy1 newline
    |> between optionalWhitespace optionalWhitespace
    |> map \lines -> {
        part1: List.map lines .part1 |> List.sum,
        part2: List.map lines .part2 |> List.sum,
    }

calibrationLine =
    oneOf [
        digit |> map \d -> NumDigit d,
        peekStringAndAdvance "one" |> map \_ -> WordDigit 1,
        peekStringAndAdvance "two" |> map \_ -> WordDigit 2,
        peekStringAndAdvance "three" |> map \_ -> WordDigit 3,
        peekStringAndAdvance "four" |> map \_ -> WordDigit 4,
        peekStringAndAdvance "five" |> map \_ -> WordDigit 5,
        peekStringAndAdvance "six" |> map \_ -> WordDigit 6,
        peekStringAndAdvance "seven" |> map \_ -> WordDigit 7,
        peekStringAndAdvance "eight" |> map \_ -> WordDigit 8,
        peekStringAndAdvance "nine" |> map \_ -> WordDigit 9,
        alpha |> map \_ -> Skip,
    ]
    |> oneOrMore
    |> map \line ->
        part1Digits = List.keepOks line \val ->
            when val is
                NumDigit d -> Ok d
                _ -> Err Skip

        part2Digits = List.keepOks line \val ->
            when val is
                NumDigit d -> Ok d
                WordDigit d -> Ok d
                _ -> Err Skip

        part1 <- calibrationValue part1Digits |> Result.try
        part2 <- calibrationValue part2Digits |> Result.map
        { part1, part2 }

calibrationValue = \digits ->
    when digits is
        [first, .., last] -> Ok (Num.toU32 (10 * first + last))
        [first] -> Ok (Num.toU32 (10 * first + first))
        [] -> Err "no digits found in line"

alpha = codeunitSatisfies \c -> 'a' <= c && c <= 'z'

newline = codeunit '\n'

isWhitespace = \char ->
    when char is
        ' ' -> Bool.true
        '\n' -> Bool.true
        '\t' -> Bool.true
        11 -> Bool.true # U+000B LINE TABULATION
        12 -> Bool.true # U+000C FORM FEED
        '\r' -> Bool.true
        _ -> Bool.false

optionalWhitespace =
    chompWhile isWhitespace

peekString = \expectedString ->
    expectedRaw = Str.toUtf8 expectedString

    buildPrimitiveParser \input ->
        if List.startsWith input expectedRaw then
            Ok { val: expectedRaw, input: input }
        else
            Err (ParsingFailure "expected input to start with string `\(expectedString)`")

peekStringAndAdvance = \expectedString ->
    const (\s -> s)
    |> keep (peekString expectedString)
    |> skip anyCodeunit
