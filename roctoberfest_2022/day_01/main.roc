app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}

import cli.Stdout
import cli.Task exposing [Task]
import "input.txt" as puzzleInput : Str
import "example.txt" as exampleInput : Str

expect topCalorieTotals exampleInput == Ok [24000, 11000, 10000]
expect topCalorieTotals puzzleInput == Ok [66186, 65638, 64980]

main =
    when topCalorieTotals puzzleInput is
        Ok [elf1, elf2, elf3, ..] ->
            total = elf1 + elf2 + elf3

            Stdout.line! "Elf 1: $(Num.toStr elf1)"
            Stdout.line! "Elf 2: $(Num.toStr elf2)"
            Stdout.line! "Elf 3: $(Num.toStr elf3)"
            Stdout.line! "Total: $(Num.toStr total)"

        Ok _ ->
            Task.err NotEnoughElves

        Err reason ->
            Task.err reason

topCalorieTotals = \input ->
    input
    |> Str.split "\n\n"
    |> List.map \elfCalories ->
        elfCalories
        |> Str.trim
        |> Str.split "\n"
        |> List.map Str.toU64
        |> combine
        |> Result.map List.sum
    |> combine
    |> Result.map List.sortDesc
    |> Result.map \totals -> List.sublist totals { start: 0, len: 3 }

combine : List (Result a e) -> Result (List a) e
combine = \list -> List.mapTry list \r -> r
