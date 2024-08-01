app "advent-2023-roc-day17"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        cli.Stdout,
        "input.txt" as puzzleInput : Str,
        "example.txt" as exampleInput : Str,
    ]
    provides [main] to cli

Node : { index: Array2D.Index, direction: [N Nat, S Nat, E Nat, W Nat] }

HeatMap : Array2D U8

main =
    Stdout.line
        """
        Part 1: \(puzzleInput |> part1 |> Num.toStr)
        """

part1 = \input ->
    input
    |> parse
    |> minimumHeatLoss

minimumHeatLoss = \heatMap ->
    start = heatMap |> Array2D.firstIndex |> orCrash "heat map empty"
    goal = heatMap |> Array2D.lastIndex |> orCrash "heat map empty"

    { heatMap, start, goal }
    |> findPath
    |> .gScores
    |> getScore goal


findPath = \config ->
    startNode = {index: start, direction: N 0}
    findPathHelp config {
        # The nodes currently at the end of potential paths
        pathNodes: [startNode],
        # For node n, prevNodes[n] is the node immediately preceding it on the
        # cheapest path from the start to n currently known.
        prevNodes: Dict.empty {},
        # For node n, gScores[n] is the cost of the cheapest path from start to n
        # currently known.
        gScores: Dict.single start 0,
        # For node n, fScores[n] = gScores[n] + heuristicScore(n). fScores[n]
        # represents our current best guess as to how cheap a path could be
        # from start to finish if it goes through n.
        fScores: Dict.single start (heuristicScore config startNode),
    }

findPathHelp = \config, state ->
    state

heuristicScore = \{goal}, {index} ->
    (Num.absDiff goal.x index.x) + (Num.absDiff goal.y index.y)

set

getScore : Dict Index F64, Index -> F64
getScore = \scores, node ->
    scores
    |> Dict.get node
    |> Result.withDefault inf

inf : F64
inf = 1 / 0
