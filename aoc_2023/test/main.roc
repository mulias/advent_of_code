app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",

    }
    imports [
        cli.Stdout,
        array2d.Array2D.{ Array2D },
        "example.txt" as exampleInput : Str,
        "input.txt" as puzzleInput : Str,
    ]
    provides [main] to cli

main : Task {} *
main = Stdout.line part1

part1 : {} -> Result Str [NotImplemented, Error Str]
part1 = \_ ->
    dbg "hello"

    lines testset
        |> List.map parseLine
        # |> List.keepIf gameIsValid
        # |> List.map .id
        # |> List.map Num.toStr
        |> List.map .cubes
        |> List.map cubesListToStr
        |> Str.joinWith "\n"
        |> Ok
