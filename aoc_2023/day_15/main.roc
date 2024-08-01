app "advent-2023-roc-day14"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, between, sepBy1, chompWhile, const, map, oneOf, buildPrimitiveParser, parsePartial, fail },
        parser.String.{ RawStr, parseStr, codeunit },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

Op [
    Set { source: Str, label: Str, focelLength: U16 },
    Dec { source: Str, label: Str },
]

main = foo

parse : Str -> Platform
parse = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser =
    opParser
    |> sepBy1 (codeunit ',')
    |> between optionalWhitespace optionalWhitespace

opParser =
    labelP
