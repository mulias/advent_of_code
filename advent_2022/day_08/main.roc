app "day08"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, map, between, chompWhile, sepBy1, oneOrMore },
        parser.String.{ RawStr, parseStr, string, digit },
        Array2D.{ Array2D },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

exampleTrees = parseInput exampleInput
puzzleTrees = parseInput puzzleInput

expect visibleTreesCount exampleTrees == 21
expect visibleTreesCount puzzleTrees == 1854

expect scenicTreeScore exampleTrees == 8
expect scenicTreeScore puzzleTrees == 527340

main =
    part1 = puzzleTrees |> visibleTreesCount |> Num.toStr
    part2 = puzzleTrees |> scenicTreeScore |> Num.toStr
    Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

Trees : Array2D { height : Nat, visible : Bool }

visibleTreesCount : Trees -> Nat
visibleTreesCount = \trees ->
    setTreeVisible = \state, tree, index -> {
        trees: Array2D.set state.trees index { tree & visible: Bool.true },
        maxHeight: tree.height,
    }

    startState = { trees, maxHeight: 0 }

    withLeftVisibility = Array2D.walk trees startState {} \state, tree, index ->
        if Array2D.isRowStart index || tree.height > state.maxHeight then
            setTreeVisible state tree index
        else
            state

    withRightVisibility = Array2D.walk trees withLeftVisibility { direction: Backwards } \state, tree, index ->
        if Array2D.isRowEnd trees index || tree.height > state.maxHeight then
            setTreeVisible state tree index
        else
            state

    withTopVisibility = Array2D.walk trees withRightVisibility { orientation: Cols } \state, tree, index ->
        if Array2D.isColStart index || tree.height > state.maxHeight then
            setTreeVisible state tree index
        else
            state

    allVisibility = Array2D.walk trees withTopVisibility { direction: Backwards, orientation: Cols } \state, tree, index ->
        if Array2D.isColEnd trees index || tree.height > state.maxHeight then
            setTreeVisible state tree index
        else
            state

    allVisibility |> .trees |> Array2D.toList |> List.countIf \{ visible } -> visible

scenicTreeScore : Trees -> Nat
scenicTreeScore = \trees ->
    Array2D.walk trees 0 {} \highScore, tree, treeIndex ->
        visibleCount = \count, otherTree, otherIndex ->
            if treeIndex == otherIndex then
                Continue (count + 1)
            else if tree.height > otherTree.height then
                Continue (count + 1)
            else
                Break count

        viewLeft = Array2D.walkUntil trees 0 { direction: BackwardsFrom treeIndex } \count, otherTree, otherIndex ->
            if Array2D.isRowStart otherIndex then
                Break count
            else
                visibleCount count otherTree otherIndex

        viewRight = Array2D.walkUntil trees 0 { direction: ForwardFrom treeIndex } \count, otherTree, otherIndex ->
            if Array2D.isRowEnd trees otherIndex then
                Break count
            else
                visibleCount count otherTree otherIndex

        viewUp = Array2D.walkUntil trees 0 { direction: BackwardsFrom treeIndex, orientation: Cols } \count, otherTree, otherIndex ->
            if Array2D.isColStart otherIndex then
                Break count
            else
                visibleCount count otherTree otherIndex

        viewDown = Array2D.walkUntil trees 0 { direction: ForwardFrom treeIndex, orientation: Cols } \count, otherTree, otherIndex ->
            if Array2D.isColEnd trees otherIndex then
                Break count
            else
                visibleCount count otherTree otherIndex

        scenicScore = viewRight * viewLeft * viewUp * viewDown

        Num.max scenicScore highScore

parseInput : Str -> Trees
parseInput = \inputStr ->
    parser =
        oneOrMore digit
        |> sepBy1 (string "\n")
        |> between optionalWhitespace optionalWhitespace
        |> map \rows ->
            rows
            |> Array2D.fromExactLists
            |> orCrash "Input rows are not all the same length"

    when parseStr parser inputStr is
        Ok treeHeights -> Array2D.map treeHeights \height -> { height, visible: Bool.false }
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

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
