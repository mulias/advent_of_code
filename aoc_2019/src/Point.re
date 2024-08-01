type t = (int, int);

type direction =
  | Up
  | Down
  | Left
  | Right;

module Comparable =
  Belt.Id.MakeComparable({
    type t = (int, int);
    let cmp = ((x1, y1), (x2, y2)) => {
      let cmpX = compare(x1, x2);
      let cmpY = compare(y1, y2);
      cmpX == 0 ? cmpY : cmpX;
    };
  });

let shift = ((x, y): t, direction: direction): t =>
  switch (direction) {
  | Up => (x, y + 1)
  | Down => (x, y - 1)
  | Left => (x - 1, y)
  | Right => (x + 1, y)
  };
