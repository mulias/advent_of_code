app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}

import cli.Stdout
import "input.txt" as puzzleInput : List U8
import "example.txt" as exampleInput : List U8

expect firstStartOfPacketMarker exampleInput == 7
expect firstStartOfMessageMarker exampleInput == 19

expect firstStartOfPacketMarker puzzleInput == 1100
expect firstStartOfMessageMarker puzzleInput == 2421

main =
    part1 = puzzleInput |> firstStartOfPacketMarker |> Num.toStr
    part2 = puzzleInput |> firstStartOfMessageMarker |> Num.toStr
    Stdout.line "Part 1: $(part1)\nPart 2: $(part2)"

CharCounts : Dict U8 U64

inc : CharCounts, U8 -> CharCounts
inc = \charCounts, char ->
    Dict.update charCounts char \possibleValue ->
        when possibleValue is
            Missing -> Present 1
            Present count -> Present (count + 1)

dec : CharCounts, U8 -> CharCounts
dec = \charCounts, char ->
    Dict.update charCounts char \possibleValue ->
        when possibleValue is
            Missing -> crash "Tried to decrement a value that wasn't present"
            Present 1 -> Missing
            Present count -> Present (count - 1)

firstStartOfPacketMarker : List U8 -> U64
firstStartOfPacketMarker = \input ->
    uniqueCharacterWindow input 4

firstStartOfMessageMarker : List U8 -> U64
firstStartOfMessageMarker = \input ->
    uniqueCharacterWindow input 14

uniqueCharacterWindow : List U8, U64 -> U64
uniqueCharacterWindow = \input, windowSize ->
    startState = { index: 0, charCounts: Dict.empty {} }
    finalState = List.walkUntil input startState \{ index, charCounts }, char ->
        nextCharCounts =
            if index >= windowSize then
                charCounts
                |> inc char
                |> dec
                    (
                        input
                        |> List.get (index - windowSize)
                        |> orCrash "index out of bounds"
                    )
            else
                inc charCounts char

        if Dict.len nextCharCounts == windowSize then
            Break { index: index + 1, charCounts: nextCharCounts }
        else
            Continue { index: index + 1, charCounts: nextCharCounts }

    finalState.index

orCrash : Result a *, Str -> a
orCrash = \result, msg ->
    when result is
        Ok a -> a
        Err _ -> crash msg
