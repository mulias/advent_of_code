open Utils;

type t('a) = Map.t(Point.t, 'a, Point.Comparable.identity);

let empty = () => Map.make(~id=(module Point.Comparable));

let rightEdge = (map: t('a)): int => {
  let xs = map->Map.keysToList->List.map(((x, _)) => x);
  List.reduce(xs, 0, (acc, x) => x > acc ? x : acc);
};

let leftEdge = (map: t('a)): int => {
  let xs = map->Map.keysToList->List.map(((x, _)) => x);
  List.reduce(xs, 0, (acc, x) => x < acc ? x : acc);
};

let topEdge = (map: t('a)): int => {
  let ys = map->Map.keysToList->List.map(((_, y)) => y);
  List.reduce(ys, 0, (acc, y) => y > acc ? y : acc);
};

let bottomEdge = (map: t('a)): int => {
  let ys = map->Map.keysToList->List.map(((_, y)) => y);
  List.reduce(ys, 0, (acc, y) => y < acc ? y : acc);
};
