app "day03"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        array2d: "../../roc-lang/roc-array2d/package/main.roc",
    }
    imports [
        cli.Stdout,
        array2d.Array2D.{ Array2D },
        "example.txt" as exampleInput : Str,
        "input.txt" as puzzleInput : Str,
    ]
    provides [main] to cli

Schematic : Array2D [Digit U8, Symbol U8, Blank]

PartNumber : {
    digits : List U8,
    indices : List Array2D.Index,
}

expect solutions exampleInput == { part1: 4361, part2: 467835 }

expect solutions puzzleInput == { part1: 512794, part2: 67779080 }

main =
    { part1, part2 } = solutions puzzleInput

    Stdout.line
        """
        Part1: \(Num.toStr part1)
        Part2: \(Num.toStr part2)
        """

solutions = \input ->
    schematic = parseSchematic input
    partNumbers = allPartNumbers schematic
    gearNumbers = gearPartNumbers schematic partNumbers

    {
        part1: partNumberSum partNumbers,
        part2: gearNumberSum gearNumbers,
    }

partNumberSum = \partNumbers ->
    partNumbers
    |> List.map partNumberToU32
    |> List.sum

gearNumberSum = \gearNumbers ->
    gearNumbers
    |> List.map \(p1, p2) ->
        (partNumberToU32 p1) * (partNumberToU32 p2)
    |> List.sum

parseSchematic : Str -> Schematic
parseSchematic = \input ->
    input
    |> Str.split "\n"
    |> List.dropLast 1
    |> List.map Str.toUtf8
    |> Array2D.fromLists FitShortest
    |> Array2D.map \elem ->
        when elem is
            '.' -> Blank
            d if isDigit d -> Digit d
            s -> Symbol s

isDigit = \d -> '0' <= d && d <= '9'

allPartNumbers : Schematic -> List PartNumber
allPartNumbers = \schematic ->
    startState = { candidates: [], current: { digits: [], indices: [] } }

    finalState = Array2D.walk
        schematic
        startState
        { direction: Forwards }
        \{ candidates, current }, elem, index ->
            when elem is
                Digit d ->
                    {
                        candidates,
                        current: {
                            digits: List.append current.digits d,
                            indices: List.append current.indices index,
                        },
                    }

                _ ->
                    {
                        candidates: List.append candidates current,
                        current: { digits: [], indices: [] },
                    }

    finalState.candidates
    |> List.append finalState.current
    |> List.keepIf \candidate ->
        anyAdjacentToSymbol schematic candidate.indices

# This version seems to spend a lof of time on needlessly refcounting
# `partNumbers`.The variable is a capture of the `walk` closure, but is not
# mutated so the compiler should be able to skip refcounting.
gearPartNumbers : Schematic, List PartNumber -> List (PartNumber, PartNumber)
gearPartNumbers = \schematic, partNumbers ->
    Array2D.walk schematic [] { direction: Forwards } \state, elem, index ->
        when elem is
            Symbol '*' ->
                adjacentPartNumbers =
                    List.keepIf partNumbers \partNumber ->
                        anyAdjacent partNumber.indices [index]

                when adjacentPartNumbers is
                    [pn1, pn2] -> List.append state (pn1, pn2)
                    _ -> state

            _ -> state

# Fast version -- by threading `partNumbers` through each loop unchanged we
# eliminate excess ref counting.
#gearPartNumbers : Schematic, List PartNumber -> List (PartNumber, PartNumber)
#gearPartNumbers = \schematic, partNumbers ->
#    Array2D.walk schematic ([], partNumbers) { direction: Forwards } \(state, pns), elem, index ->
#        when elem is
#            Symbol '*' ->
#                adjacentPartNumbers =
#                    List.keepIf pns \partNumber ->
#                        anyAdjacent partNumber.indices [index]
#
#                when adjacentPartNumbers is
#                    [pn1, pn2] -> (List.append state (pn1, pn2), pns)
#                    _ -> (state, pns)
#
#            _ -> (state, pns)
#    |> .0

partNumberToU32 = \{ digits } ->
    digits
    |> Str.fromUtf8
    |> Result.try Str.toU32
    |> Result.withDefault 0

isAdjacent = \a, b ->
    deltaX = Num.absDiff a.x b.x
    deltaY = Num.absDiff a.y b.y
    a != b && deltaX <= 1 && deltaY <= 1

anyAdjacent = \aa, bb ->
    a <- List.any aa
    b <- List.any bb
    isAdjacent a b

anyAdjacentToSymbol = \schematic, indices ->
    List.any indices \index -> isAdjacentToSymbol schematic index

isAdjacentToSymbol = \schematic, index ->
    adjacentIndex <- List.any [
            index |> incX,
            index |> incY,
            index |> decX,
            index |> decY,
            index |> incX |> incY,
            index |> incX |> decY,
            index |> decX |> incY,
            index |> decX |> decY,
        ]
    when Array2D.get schematic adjacentIndex is
        Ok (Symbol _) -> Bool.true
        _ -> Bool.false

incX = \{ x, y } -> { x: Num.addWrap x 1, y }
decX = \{ x, y } -> { x: Num.subWrap x 1, y }
incY = \{ x, y } -> { x, y: Num.addWrap y 1 }
decY = \{ x, y } -> { x, y: Num.subWrap y 1 }
