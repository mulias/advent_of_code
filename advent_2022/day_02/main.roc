app "day02"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "../Parser/main.roc",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, const, between, keep, skip, sepBy, oneOf, map },
        parser.String.{ parseStr, codeunit },
        parser.Whitespace.{ optionalWhitespace },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

main =
    part1 = puzzleTournament |> tournamentScorePart1 |> Num.toStr
    part2 = puzzleTournament |> tournamentScorePart2 |> Num.toStr
    Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

exampleTournament = parseTournament exampleInput
puzzleTournament = parseTournament puzzleInput

expect tournamentScorePart1 exampleTournament == 15
expect tournamentScorePart1 puzzleTournament == 10595

expect tournamentScorePart2 exampleTournament == 12
expect tournamentScorePart2 puzzleTournament == 9541

Shape : [Rock, Scissors, Paper]

Outcome : [Loss, Draw, Win]

Round : {
    playerHand : Shape,
    playerGoal : Outcome,
    opponentHand : Shape,
}

tournamentScorePart1 = \tournament ->
    tournament
    |> List.map \round ->
        shape = round.playerHand
        outcome = roundOutcome shape round.opponentHand
        roundScore shape outcome
    |> List.sum

tournamentScorePart2 = \tournament ->
    tournament
    |> List.map \round ->
        shape = roundChoice round
        outcome = roundOutcome shape round.opponentHand
        roundScore shape outcome
    |> List.sum

roundOutcome : Shape, Shape -> Outcome
roundOutcome = \playerHand, opponentHand ->
    when (playerHand, opponentHand) is
        (Rock, Rock) -> Draw
        (Rock, Paper) -> Loss
        (Rock, Scissors) -> Win
        (Paper, Rock) -> Win
        (Paper, Paper) -> Draw
        (Paper, Scissors) -> Loss
        (Scissors, Rock) -> Loss
        (Scissors, Paper) -> Win
        (Scissors, Scissors) -> Draw

roundChoice : Round -> Shape
roundChoice = \{ playerGoal, opponentHand } ->
    if roundOutcome Rock opponentHand == playerGoal then
        Rock
    else if roundOutcome Paper opponentHand == playerGoal then
        Paper
    else
        Scissors

roundScore : Shape, Outcome -> U64
roundScore = \shape, outcome ->
    shapeScore =
        when shape is
            Rock -> 1
            Paper -> 2
            Scissors -> 3

    outcomeScore =
        when outcome is
            Loss -> 0
            Draw -> 3
            Win -> 6

    shapeScore + outcomeScore

parseTournament : Str -> List Round
parseTournament = \input ->
    when parseStr tournamentParser input is
        Ok tournament -> tournament
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

tournamentParser =
    roundParser
    |> sepBy optionalWhitespace
    |> between optionalWhitespace optionalWhitespace

roundParser =
    const (\opponentHand -> \{ playerHand, playerGoal } -> { playerHand, playerGoal, opponentHand })
    |> keep opponentParser
    |> skip (codeunit ' ')
    |> keep playerParser

opponentParser =
    oneOf [
        codeunit 'A' |> map \_ -> Rock,
        codeunit 'B' |> map \_ -> Paper,
        codeunit 'C' |> map \_ -> Scissors,
    ]

playerParser =
    oneOf [
        codeunit 'X' |> map \_ -> { playerHand: Rock, playerGoal: Loss },
        codeunit 'Y' |> map \_ -> { playerHand: Paper, playerGoal: Draw },
        codeunit 'Z' |> map \_ -> { playerHand: Scissors, playerGoal: Win },
    ]
