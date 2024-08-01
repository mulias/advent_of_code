open Utils;

type color =
  | Black
  | White;

type t = PointMap.t(color);

let blank = () => PointMap.empty();

let panelColor = (hull: t, pos: Point.t): color =>
  Map.get(hull, pos)->Option.getWithDefault(Black);

let paintPanel = (hull: t, pos: Point.t, color: color): t =>
  Map.set(hull, pos, color);

let showRow = (hull: t, xStart: int, xEnd: int, y: int): string => {
  let rowPos = List.range(xStart, xEnd)->List.map(x => (x, y));

  List.map(rowPos, panelColor(hull))
  ->List.map(color => color == Black ? "." : "#")
  ->List.joinWith("");
};

let show = (hull: t): string => {
  let padding = 2;
  let right = PointMap.rightEdge(hull) + padding;
  let left = PointMap.leftEdge(hull) - padding;
  let top = PointMap.topEdge(hull) + padding;
  let bottom = PointMap.bottomEdge(hull) - padding;

  List.range(bottom, top)
  ->List.reverse
  ->List.map(showRow(hull, left, right))
  ->List.joinWith("\n");
};
