open Utils;

type tile =
  | Empty
  | Wall
  | Block
  | Paddle
  | Ball;

[@bs.deriving accessors]
type t = {
  screen: PointMap.t(tile),
  score: int,
};

let init = () => {screen: PointMap.empty(), score: 0};

let getTile = ({screen}: t, pos: Point.t): tile =>
  Map.get(screen, pos)->Option.getWithDefault(Empty);

let setTile = (game: t, pos: Point.t, tile: tile): t => {
  ...game,
  screen: Map.set(game.screen, pos, tile),
};

let setScore = (game: t, score: int): t => {...game, score};

let findFirst = (game: t, target: tile): option(Point.t) =>
  Map.findFirstBy(game.screen, (_, tile) => tile == target)
  ->Option.map(((pos, _)) => pos);

let showTile = (tile: tile): string =>
  switch (tile) {
  | Empty => " "
  | Wall => "#"
  | Block => "="
  | Paddle => "-"
  | Ball => "*"
  };

let showRow = (game: t, xStart: int, xEnd: int, y: int): string => {
  let rowPos = List.range(xStart, xEnd)->List.map(x => (x, y));

  List.map(rowPos, getTile(game))->List.map(showTile)->List.joinWith("");
};

let show = (game: t): string => {
  let {screen, score} = game;
  let right = PointMap.rightEdge(screen);
  let left = PointMap.leftEdge(screen);
  let top = PointMap.topEdge(screen);
  let bottom = PointMap.bottomEdge(screen);

  let showScreen =
    List.range(bottom, top)
    ->List.map(showRow(game, left, right))
    ->List.joinWith("\n");

  let showScore = "\n score: " ++ string_of_int(score) ++ "\n";

  showScreen ++ showScore;
};
