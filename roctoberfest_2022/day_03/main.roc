app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import cli.Stdout
import parser.Core exposing [Parser, between, sepBy, map, chompUntil, chompWhile]
import parser.String exposing [parseStr]
import "input.txt" as puzzleInput : Str
import "example.txt" as exampleInput : Str

main =
    part1 = puzzleRucksacks |> sharedItemPrioritiesSum |> Num.toStr
    part2 = puzzleRucksacks |> groupSharedItemPrioritiesSum |> Num.toStr
    Stdout.line "Part 1: $(part1)\nPart 2: $(part2)"

exampleRucksacks = parseRucksacks exampleInput
puzzleRucksacks = parseRucksacks puzzleInput

expect sharedItemPrioritiesSum exampleRucksacks == 157
expect sharedItemPrioritiesSum puzzleRucksacks == 7831

expect groupSharedItemPrioritiesSum exampleRucksacks == 70
expect groupSharedItemPrioritiesSum puzzleRucksacks == 2683

Item : U8

Rucksack : (Set Item, Set Item)

sharedItemPrioritiesSum : List Rucksack -> U64
sharedItemPrioritiesSum = \rucksacks ->
    rucksacks |> sharedItemPriorities |> List.map Num.intCast |> List.sum

groupSharedItemPrioritiesSum : List Rucksack -> U64
groupSharedItemPrioritiesSum = \rucksacks ->
    rucksacks |> groupSharedItemPriorities |> List.map Num.intCast |> List.sum

sharedItemPriorities : List Rucksack -> List U8
sharedItemPriorities = \rucksacks ->
    List.map rucksacks \(left, right) ->
        shared = Set.intersection left right |> Set.toList

        when shared is
            [item] -> itemPriority item
            _ -> crash "Each rucksack should have exactly one overlapping item between compartments"

groupSharedItemPriorities : List Rucksack -> List U8
groupSharedItemPriorities = \rucksacks ->
    rucksacks
    |> List.map allItems
    |> List.chunksOf 3
    |> List.map \group ->
        shared = group |> intersectAll |> Set.toList

        when shared is
            [item] -> itemPriority item
            _ -> crash "Each group of three should have exactly one overlapping item across rucksacks"

itemPriority : Item -> U8
itemPriority = \item ->
    if 'a' <= item && item <= 'z' then
        item + 1 - 'a'
    else if 'A' <= item && item <= 'Z' then
        item + 27 - 'A'
    else
        crash "Unexpected item '$(Num.toStr item)'"

allItems : Rucksack -> Set Item
allItems = \(left, right) -> Set.union left right

intersectAll : List (Set a) -> Set a
intersectAll = \sets ->
    when sets is
        [] -> Set.empty {}
        [first, ..] ->
            List.walk (List.dropFirst sets 1) first \acc, items -> Set.intersection acc items

parseRucksacks : Str -> List Rucksack
parseRucksacks = \input ->
    when parseStr inputParser input is
        Ok rucksacks -> rucksacks
        Err (ParsingFailure msg) -> crash "parsing failure '$(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '$(leftover)'"

inputParser =
    rucksackParser
    |> sepBy optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

rucksackParser =
    chompUntil '\n'
    |> map \items ->
        halfway = Num.divTrunc (List.len items) 2
        { before, others } = List.split items halfway
        (Set.fromList before, Set.fromList others)

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
