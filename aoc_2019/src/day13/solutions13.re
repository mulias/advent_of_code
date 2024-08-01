open Utils;

let intToTile = (i: int): Game.tile =>
  Game.(
    switch (i) {
    | 0 => Empty
    | 1 => Wall
    | 2 => Block
    | 3 => Paddle
    | 4 => Ball
    | _ => raise(Failure("Invalid tile id"))
    }
  );

let readTile = (state: Program.t): (Point.t, Game.tile) => {
  let x = Program.readOutputInt(state);
  let y = Program.readOutputInt(state);
  let tileId = Program.readOutputInt(state);

  ((x, y), intToTile(tileId));
};

let readScore = (state: Program.t): int => {
  let _ = Program.readOutputInt(state);
  let _ = Program.readOutputInt(state);
  let score = Program.readOutputInt(state);
  score;
};

let rec stepGameState =
        (program: Program.t, game: Game.t): (Program.t, Game.t) => {
  let program = Computer.runUntilBlocked(program);

  switch (Program.getOutputInts(program)) {
  | [(-1), 0, _score] => (program, Game.setScore(game, readScore(program)))
  | [(-1), 0, _score, ..._] =>
    stepGameState(program, Game.setScore(game, readScore(program)))
  | [_, _, _, ..._] =>
    let (pos, tile) = readTile(program);
    stepGameState(program, Game.setTile(game, pos, tile));
  | _ => (program, game)
  };
};

// DAY 13 PART 1

let gameProgram =
  Program.readProgramFile("src/day13/input.txt")->Program.makeExecutable;

let (_, firstGameState) = stepGameState(gameProgram, Game.init());

let answer =
  firstGameState.screen->Map.keep((_, tile) => tile == Game.Block)->Map.size;

let test1 = () => assert(answer == 200);

// DAY 13 PART 2

type move =
  | Left
  | Neutral
  | Right;

let movePadel = (program: Program.t, move: move): Program.t =>
  Program.writeInputInt(
    program,
    switch (move) {
    | Left => (-1)
    | Neutral => 0
    | Right => 1
    },
  );

let gameProgram =
  Program.readProgramFile("src/day13/input.txt")
  ->Memory.write(Memory.Address.of_int(0), Memory.Value.of_int(2))
  ->Program.makeExecutable;

let rec runGame = (program: Program.t, game: Game.t): int => {
  let (program, game) = stepGameState(program, game);
  let (ballX, _) =
    Game.findFirst(game, Game.Ball)->Option.getWithDefault((0, 0));
  let (paddleX, _) =
    Game.findFirst(game, Game.Paddle)->Option.getWithDefault((0, 0));
  let program =
    if (ballX < paddleX) {
      movePadel(program, Left);
    } else if (ballX > paddleX) {
      movePadel(program, Right);
    } else {
      movePadel(program, Neutral);
    };

  /* Js.log(Game.show(game)); */

  if (Computer.isHalted(program)) {
    game.score;
  } else {
    runGame(program, game);
  };
};

let answer = runGame(gameProgram, Game.init());

let test2 = () => assert(answer == 9803);
