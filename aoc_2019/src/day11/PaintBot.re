type rotation =
  | Left
  | Right;

type orientation =
  | Up
  | Down
  | Right
  | Left;

type t = {
  pos: (int, int),
  orientation,
  hull: ShipHull.t,
};

let start = (): t => {pos: (0, 0), orientation: Up, hull: ShipHull.blank()};

let paintPanel = (bot: t, color: ShipHull.color): t => {
  ...bot,
  hull: ShipHull.paintPanel(bot.hull, bot.pos, color),
};

let rotate = (bot: t, turn: rotation): t => {
  let orientation =
    switch (bot.orientation, turn) {
    | (Up, Left) => Left
    | (Up, Right) => Right
    | (Down, Left) => Right
    | (Down, Right) => Left
    | (Right, Left) => Up
    | (Right, Right) => Down
    | (Left, Left) => Down
    | (Left, Right) => Up
    };
  {...bot, orientation};
};

let move = (bot: t): t => {
  let {pos: (x, y), orientation} = bot;
  switch (orientation) {
  | Up => {...bot, pos: (x, y + 1)}
  | Down => {...bot, pos: (x, y - 1)}
  | Right => {...bot, pos: (x + 1, y)}
  | Left => {...bot, pos: (x - 1, y)}
  };
};

let step = (bot: t, color: ShipHull.color, turn: rotation): t =>
  bot->paintPanel(color)->rotate(turn)->move;
