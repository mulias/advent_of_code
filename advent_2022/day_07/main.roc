app "day07"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, parsePartial, buildPrimitiveParser, many, between, chompWhile, chompUntil, keep, skip, const, maybe },
        parser.String.{ RawStr, parseStr, string, digits },
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

main =
    dirSizes = sizes puzzleRootDir
    part1 = dirSizes |> sumOfSmallDirs |> Num.toStr
    part2 = dirSizes |> sizeOfDirToDelete |> Num.toStr
    Stdout.line "Part 1: \(part1)\nPart 2: \(part2)"

exampleRootDir = parseInput exampleInput
puzzleRootDir = parseInput puzzleInput

expect exampleRootDir |> sizes |> sumOfSmallDirs == 95437
expect exampleRootDir |> sizes |> sizeOfDirToDelete == 24933642

expect puzzleRootDir |> sizes |> sumOfSmallDirs == 1501149
expect puzzleRootDir |> sizes |> sizeOfDirToDelete == 10096985

Dir : [
    Branch { files : List Nat, subDirs : List Dir },
    Leaf { files : List Nat },
]

DirSizes : {
    directDirTotal : Nat,
    directSubDirTotals : List Nat,
    indirectSubDirTotals : List Nat,
}

smallDirSize = 100000
requiredSize = 30000000
filesystemSize = 70000000

sizes : Dir -> DirSizes
sizes = \dir ->
    when dir is
        Branch { files, subDirs } ->
            subDirSizes = List.map subDirs sizes

            subDirDirectSubDirTotals = subDirSizes |> List.map .directSubDirTotals |> List.join
            subDirIndirectSubDirTotals = subDirSizes |> List.map .indirectSubDirTotals |> List.join

            {
                directDirTotal: List.sum files,
                directSubDirTotals: List.map subDirSizes size,
                indirectSubDirTotals: List.concat subDirDirectSubDirTotals subDirIndirectSubDirTotals,
            }

        Leaf { files } ->
            {
                directDirTotal: List.sum files,
                directSubDirTotals: [],
                indirectSubDirTotals: [],
            }

allSizes : DirSizes -> List Nat
allSizes = \dirSizes ->
    List.join [
        [size dirSizes],
        dirSizes.directSubDirTotals,
        dirSizes.indirectSubDirTotals,
    ]

size : DirSizes -> Nat
size = \{ directDirTotal, directSubDirTotals } -> directDirTotal + (List.sum directSubDirTotals)

sumOfSmallDirs : DirSizes -> Nat
sumOfSmallDirs = \dirSizes ->
    dirSizes
    |> allSizes
    |> List.keepIf \dirSize -> dirSize <= smallDirSize
    |> List.sum

sizeOfDirToDelete : DirSizes -> Nat
sizeOfDirToDelete = \dirSizes ->
    rootDirSize = size dirSizes
    requiredSpace = requiredSize - (filesystemSize - rootDirSize)

    dirSizes
    |> allSizes
    |> List.keepIf \dirSize -> dirSize >= requiredSpace
    |> List.sortAsc
    |> List.first
    |> Result.withDefault 0

newDir : List Nat, List Dir -> Dir
newDir = \files, subDirs ->
    if List.isEmpty subDirs then
        Leaf { files }
    else
        Branch { files, subDirs }

parseInput : Str -> Dir
parseInput = \inputStr ->
    when parseStr inputParser inputStr is
        Ok input -> input
        Err (ParsingFailure msg) -> crash "parsing failure '\(msg)'"
        Err (ParsingIncomplete leftover) -> crash "parsing incomplete '\(leftover)'"

inputParser : Parser RawStr Dir
inputParser = dirParser |> between optionalWhitespace optionalWhitespace

dirParser : Parser RawStr Dir
dirParser =
    const (\files -> \subDirs -> newDir files subDirs)
    |> skip dirNameParser
    |> keep filesParser
    |> keep subDirsParser
    |> skip (maybe dirBackParser)
    |> skip optionalWhitespace

dirNameParser : Parser RawStr RawStr
dirNameParser =
    const (\name -> name)
    |> skip (string "$ cd ")
    |> keep (chompUntil '\n')
    |> skip optionalWhitespace

filesParser : Parser RawStr (List Nat)
filesParser =
    const (\fileSizes -> fileSizes)
    |> skip (string "$ ls\n")
    |> skip (many dirListingParser)
    |> keep (many fileSizeParser)
    |> skip (many dirListingParser)
    |> skip optionalWhitespace

fileSizeParser : Parser RawStr Nat
fileSizeParser =
    const (\fileSize -> fileSize)
    |> skip (many dirListingParser)
    |> keep digits
    |> skip (chompUntil '\n')
    |> skip optionalWhitespace
    |> skip (many dirListingParser)

dirListingParser : Parser RawStr RawStr
dirListingParser =
    const (\name -> name)
    |> skip optionalWhitespace
    |> skip (string "dir ")
    |> keep (chompUntil '\n')
    |> skip optionalWhitespace

subDirsParser : Parser RawStr (List Dir)
subDirsParser =
    buildPrimitiveParser \input ->
        parsePartial (manyUntil dirParser dirBackParser) input

dirBackParser = string "$ cd .."

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

manyUntil : Parser input a, Parser input * -> Parser input (List a)
manyUntil = \parser, untilParser ->
    many (unless parser untilParser)

unless : Parser input a, Parser input * -> Parser input a
unless = \parser, unlessParser ->
    buildPrimitiveParser \input ->
        when parsePartial unlessParser input is
            Err (ParsingFailure _) ->
                parsePartial parser input

            Ok _ ->
                Err (ParsingFailure "Succeeded with `unless` parser")

expect
    parseStr (string "foobar" |> unless (string "foo")) "foobar"
    == Err (ParsingFailure "Succeeded with `unless` parser")

expect
    parseInput exampleInput
    == Branch {
        files: [14848514, 8504156],
        subDirs: [
            Branch {
                files: [29116, 2557, 62596],
                subDirs: [
                    Leaf { files: [584] },
                ],
            },
            Leaf {
                files: [4060174, 8033020, 5626152, 7214296],
            },
        ],
    }
