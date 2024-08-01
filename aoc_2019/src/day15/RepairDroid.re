open Utils;

type tile =
  | Empty
  | Floor
  | Wall
  | Droid;

type direction = Point.direction;

type t = {
  pos: Point.t,
  ship: PointMap.t(tile),
};

let getTile = ({ship}: t, pos: Point.t): tile =>
  Map.get(ship, pos)->Option.getWithDefault(Empty);

let setTile = (droid: t, pos: Point.t, tile: tile): t => {
  ...droid,
  ship: Map.set(droid.ship, pos, tile),
};

let moveDroid = (droid: t, newPos: Point.t): t =>
  {...droid, pos: newPos}
  ->setTile(droid.pos, Floor)
  ->setTile(newPos, Droid);

let start = () =>
  {pos: (0, 0), ship: PointMap.empty()}->setTile((0, 0), Droid);

let showTile = (tile: tile): string =>
  switch (tile) {
  | Empty => " "
  | Floor => "."
  | Wall => "#"
  | Droid => "D"
  };

let showRow = (droid: t, xStart: int, xEnd: int, y: int): string => {
  let rowPos = List.range(xStart, xEnd)->List.map(x => (x, y));

  List.map(rowPos, getTile(droid))->List.map(showTile)->List.joinWith("");
};

let show = (droid: t): string => {
  let {pos, ship} = droid;

  let right = PointMap.rightEdge(ship);
  let left = PointMap.leftEdge(ship);
  let top = PointMap.topEdge(ship);
  let bottom = PointMap.bottomEdge(ship);

  let map =
    List.range(bottom, top)
    ->List.reverse
    ->List.map(showRow(droid, left, right))
    ->List.joinWith("\n");

  "\n" ++ map ++ "\n";
};
