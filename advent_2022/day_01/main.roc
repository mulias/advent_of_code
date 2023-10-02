app "day01"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        cli.Stdout,
        cli.Task,
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

main =
    when topCalorieTotals puzzleInput is
        Ok ([elf1, elf2, elf3]) ->
            total = elf1 + elf2 + elf3

            _ <- Stdout.line "Elf 1: \(Num.toStr elf1)" |> Task.await
            _ <- Stdout.line "Elf 2: \(Num.toStr elf2)" |> Task.await
            _ <- Stdout.line "Elf 3: \(Num.toStr elf3)" |> Task.await
            _ <- Stdout.line "Total: \(Num.toStr total)" |> Task.await
            Task.ok {}

        _ ->
            Stdout.line "Error"

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
    |> Result.map \totals -> List.sublist totals {start: 0, len: 3}

combine : List (Result a e) -> Result (List a) e
combine = \list -> List.mapTry list \r -> r

expect topCalorieTotals exampleInput == Ok [24000, 11000, 10000]
expect topCalorieTotals puzzleInput == Ok [66186, 65638, 64980]
