app "advent-2023-roc-day07"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        "input-full.txt" as inputFull : Str,
        "input-sample.txt" as inputSample : Str,
        "input-sample-alt.txt" as inputSampleAlt : Str,
        parser.String.{ RawStr, parseStr, string, digits },
        parser.Core.{ Parser, apply, const, skip, maybe, chompUntil, sepBy },
    ]
    provides [main] to pf

Card : U8

Hand : { cards: List Card, bid: Nat }

main : Task {} *
main =
    _ <- Stdout.line "Part 1: \(Num.toStr (part1 inputFull))" |> Task.await
    Stdout.line "Part 2: \(Num.toStr (part2 inputFull))"

#
# PART 1
#
part1 : Str -> Nat
part1 = \input ->
    values = cardValues "23456789TJQKA"
    input
    |> parseHands
    |> List.map \hands -> generateHandStrength hands values
    |> sortBy .strength
    |> List.mapWithIndex \hand, index -> hand.bid * (index + 1)
    |> List.sum

expect
    result = part1 inputSample
    result == 6440

# From https://www.reddit.com/r/adventofcode/comments/18cr4xr/2023_day_7_better_example_input_not_a_spoiler/
expect
    result = part1 inputSampleAlt
    result == 6592

parseHands : Str -> List Hand
parseHands = \input ->
    when parseStr handsParser input is
        Ok parsed -> parsed
        Err (ParsingFailure str) -> crash "Invalid input: \(str)"
        Err (ParsingIncomplete str) -> crash "Incomplete input: \(str)"

handsParser : Parser (List U8) (List Hand)
handsParser =
    handParser
    |> sepBy (string "\n")
    |> skip (maybe (string "\n"))

handParser : Parser (List U8) Hand
handParser =
    const (\cards -> \bid -> { cards: cards, bid })
    |> apply (chompUntil ' ')
    |> skip (string " ")
    |> apply digits

cardValues : Str -> Dict U64 U64
cardValues = \allCards ->
    allCards
    |> Str.toScalars
    |> List.map Num.toU64
    |> List.walkWithIndex (Dict.empty {}) \dict, card, index ->
        Dict.insert dict card (Num.toU64 index)

generateHandStrength : Hand, Dict U64 U64 -> { strength: U64, bid: Nat }
generateHandStrength = \hand, values ->
    strength = handStrength (hand.cards |> List.map Num.toU64) values
    # cardsStr = hand.cards |> Str.fromUtf8 |> okOrCrash "Unable to convert cards back to string"
    { strength, bid: hand.bid }

handStrength = \cards, values ->
    cards
    |> List.map \card -> (Dict.get values card) |> okOrCrash "Invalid card \(Num.toStr card)"
    |> List.prepend (handTypeValue (handTypeCounts cards))
    |> List.reverse
    |> List.walkWithIndex 0 \sum, value, index ->
        sum + value * (Num.powInt 16 (Num.toU64 index))

handTypeCounts = \cards ->
    cards
    |> List.walk (Dict.empty {}) \dict, card ->
        when Dict.get dict card is
            Ok amount -> Dict.insert dict card (amount + 1)
            Err KeyNotFound -> Dict.insert dict card 1
    |> Dict.values
    |> List.sortDesc

handTypeValue = \counts ->
    when counts is
        [1, 1, 1, 1, 1] -> 0
        [2, 1, 1, 1] -> 1
        [2, 2, 1] -> 2
        [3, 1, 1] -> 3
        [3, 2] -> 4
        [4, 1] -> 5
        [5] -> 6
        _ -> crash "Invalid poker hand"

expect
    result = handTypeCounts ("AAAAA" |> Str.toScalars |> List.map Num.toU64)
    result == [5]

expect
    result = handTypeCounts ("KKAAA" |> Str.toScalars |> List.map Num.toU64)
    result == [3, 2]

expect
    result = handTypeCounts ("AKAQA" |> Str.toScalars |> List.map Num.toU64)
    result == [3, 1, 1]

expect
    result = handTypeCounts ("AKQJT" |> Str.toScalars |> List.map Num.toU64)
    result == [1, 1, 1, 1, 1]

sortBy = \list, func ->
    List.sortWith list \a, b ->
        aVal = func a
        bVal = func b
        if aVal < bVal then
            LT
        else if aVal > bVal then
            GT
        else
            EQ

okOrCrash = \result, error ->
    when result is
        Ok value -> value
        Err _ -> crash error

#
# PART 2
#
part2 = \input ->
    values = cardValues "J23456789TQKA"
    input
    |> parseHands
    |> List.map \hands -> generateHandStrengthWithJokers hands values
    |> sortBy .strength
    |> List.mapWithIndex \hand, index -> hand.bid * (index + 1)
    |> List.sum

expect
    result = part2 inputSample
    result == 5905

expect
    result = part2 inputSampleAlt
    result == 6839

generateHandStrengthWithJokers = \hand, values ->
    strength = handStrengthWithJokers (hand.cards |> List.map Num.toU64) values
    # cardsStr = hand.cards |> Str.fromUtf8 |> okOrCrash "Unable to convert cards back to string"
    { strength, bid: hand.bid }

handStrengthWithJokers = \cards, values ->
    cards
    |> List.map \card -> (Dict.get values card) |> okOrCrash "Invalid card \(Num.toStr card)"
    |> List.prepend (handTypeValue (handTypeCountsWithJokers cards))
    |> List.reverse
    |> List.walkWithIndex 0 \sum, value, index ->
        sum + value * (Num.powInt 16 (Num.toU64 index))

handTypeCountsWithJokers = \cards ->
    list =
        cards
        |> List.dropIf \card -> card == 'J'
        |> List.walk (Dict.empty {}) \dict, card ->
            when Dict.get dict card is
                Ok amount -> Dict.insert dict card (amount + 1)
                Err KeyNotFound -> Dict.insert dict card 1
        |> Dict.values
        |> List.sortDesc
    jokersCount = cards |> List.countIf \card -> card == 'J'
    when List.first list is
        Ok first -> List.set list 0 (jokersCount + first)
        Err ListWasEmpty -> [5] # All jokers

expect
    result = handTypeCountsWithJokers ("AAAAJ" |> Str.toScalars |> List.map Num.toU64)
    result == [5]

expect
    result = handTypeCountsWithJokers ("AJJJJ" |> Str.toScalars |> List.map Num.toU64)
    result == [5]

expect
    result = handTypeCountsWithJokers ("AKJJJ" |> Str.toScalars |> List.map Num.toU64)
    result == [4, 1]

expect
    result = handTypeCountsWithJokers ("AKQJJ" |> Str.toScalars |> List.map Num.toU64)
    result == [3, 1, 1]

expect
    result = handTypeCountsWithJokers ("AKQTJ" |> Str.toScalars |> List.map Num.toU64)
    result == [2, 1, 1, 1]
