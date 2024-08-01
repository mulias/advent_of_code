app "advent-2023-roc-day13"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
        array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.1.0/ssMT0bDIv-qE7d_yNUyCByGQHvpNkQJZsGUS6xEFsIY.tar.br",
    }
    imports [
        cli.Stdout,
        array2d.Array2D.{ Array2D, Index, Shape },
        parser.Core.{ Parser, between, sepBy1, chompWhile, const, map, oneOf, buildPrimitiveParser, parsePartial, fail },
        parser.String.{ RawStr, parseStr, string, codeunit },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

expect part1 exampleInput == 405
expect part2 exampleInput == 400

expect part1 puzzleInput == 33356
expect part2 puzzleInput == 28475

Pattern : Array2D [Ash, Rock]

Line : [X Nat, Y Nat]

main =
    Stdout.line
        """
        Part 1: \(puzzleInput |> part1 |> Num.toStr)
        Part 2: \(puzzleInput |> part2 |> Num.toStr)
        """

part1 = \input ->
    input
    |> parseInput
    |> List.map \pattern ->
        pattern
        |> findLineOfReflection
        |> orCrash "No reflection"
        |> summarizeLine
    |> List.sum

part2 = \input ->
    input
    |> parseInput
    |> List.map \pattern ->
        pattern
        |> findSmudgedLineOfReflection
        |> orCrash "No reflection"
        |> summarizeLine
    |> List.sum

summarizeLine : Line -> Nat
summarizeLine = \line ->
    when line is
        X n -> n * 100
        Y n -> n

findLineOfReflection : Pattern -> Result Line [NotFound]
findLineOfReflection = \pattern ->
    pattern
    |> allLines
    |> List.findFirst \line -> isLineOfReflection pattern line

findSmudgedLineOfReflection : Pattern -> Result Line [NotFound]
findSmudgedLineOfReflection = \pattern ->
    Array2D.walkUntil pattern (Err NotFound) { direction: Forwards } \_state, elem, index ->
        smudgedElem = if elem == Ash then Rock else Ash
        smudgedPattern = Array2D.set pattern index smudgedElem

        nextState =
            smudgedPattern
            |> allLines
            |> List.findFirst \line ->
                indexInReflection =
                    index
                    |> reflectIndex line
                    |> Result.map \reflectionIndex ->
                        Array2D.hasIndex pattern reflectionIndex
                    |> Result.withDefault Bool.false

                indexInReflection && isLineOfReflection smudgedPattern line

        if Result.isOk nextState then Break nextState else Continue nextState

allLines : Pattern -> List Line
allLines = \pattern ->
    { dimX, dimY } = Array2D.shape pattern

    linesX =
        { start: At 1, end: Before dimX }
        |> List.range
        |> List.map X

    linesY =
        { start: At 1, end: Before dimY }
        |> List.range
        |> List.map Y

    List.concat linesX linesY

isLineOfReflection : Pattern, Line -> Bool
isLineOfReflection = \pattern, line ->
    Array2D.walkUntil pattern Bool.true { direction: Forwards } \_state, elem, index ->
        index
        |> reflectIndex line
        |> Result.try \reflectionIndex -> Array2D.get pattern reflectionIndex
        |> Result.map \reflectionElem -> elem == reflectionElem
        |> Result.withDefault Bool.true
        |> \isReflection ->
            if isReflection then Continue Bool.true else Break Bool.false

reflectIndex : Index, Line -> Result Index [OutOfBounds]
reflectIndex = \{ x, y }, lineOfReflection ->
    when lineOfReflection is
        X n ->
            if x >= n then
                ((Num.absDiff x n) + 1)
                |> \distFromLine -> Num.subChecked n distFromLine
                |> Result.map \reflectedX -> { x: reflectedX, y }
                |> Result.mapErr \_ -> OutOfBounds
            else
                Num.absDiff x n
                |> Num.subChecked 1
                |> Result.map \distFromLine -> { x: n + distFromLine, y }
                |> Result.mapErr \_ -> OutOfBounds

        Y n ->
            if y >= n then
                ((Num.absDiff y n) + 1)
                |> \distFromLine -> Num.subChecked n distFromLine
                |> Result.map \reflectedY -> { x, y: reflectedY }
                |> Result.mapErr \_ -> OutOfBounds
            else
                Num.absDiff y n
                |> Num.subChecked 1
                |> Result.map \distFromLine -> { x, y: n + distFromLine }
                |> Result.mapErr \_ -> OutOfBounds

expect reflectIndex { x: 0, y: 5 } (X 5) == Ok { x: 9, y: 5 }
expect reflectIndex { x: 1, y: 4 } (X 5) == Ok { x: 8, y: 4 }
expect reflectIndex { x: 2, y: 3 } (X 5) == Ok { x: 7, y: 3 }
expect reflectIndex { x: 3, y: 2 } (X 5) == Ok { x: 6, y: 2 }
expect reflectIndex { x: 4, y: 1 } (X 5) == Ok { x: 5, y: 1 }
expect reflectIndex { x: 5, y: 0 } (X 5) == Ok { x: 4, y: 0 }
expect reflectIndex { x: 0, y: 5 } (Y 2) == Err OutOfBounds
expect reflectIndex { x: 1, y: 4 } (Y 2) == Err OutOfBounds
expect reflectIndex { x: 2, y: 3 } (Y 2) == Ok { x: 2, y: 0 }
expect reflectIndex { x: 3, y: 2 } (Y 2) == Ok { x: 3, y: 1 }
expect reflectIndex { x: 4, y: 1 } (Y 2) == Ok { x: 4, y: 2 }
expect reflectIndex { x: 5, y: 0 } (Y 2) == Ok { x: 5, y: 3 }

parseInput : Str -> List Pattern
parseInput = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser =
    patternParser
    |> sepBy1 (string "\n\n")
    |> between optionalWhitespace optionalWhitespace

patternParser =
    table
        (oneOf [ashParser, rockParser])
        (const NoElemSep)
        (codeunit '\n')

ashParser = codeunit '.' |> map \_ -> Ash

rockParser = codeunit '#' |> map \_ -> Rock

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

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
