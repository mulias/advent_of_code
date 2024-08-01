open Belt;

let paintProgram =
  Program.readProgramFile("src/day11/input.txt")->Program.makeExecutable;

let robot = PaintBot.start();

let colorToValue = (color: ShipHull.color): Memory.value =>
  switch (color) {
  | Black => Int64.zero
  | White => Int64.one
  };

let valueToColor = (value: Memory.value): ShipHull.color =>
  switch (Int64.to_int(value)) {
  | 0 => Black
  | 1 => White
  | _ => raise(Failure("Program output is not a valid color"))
  };

let valueToRotation = (value: Memory.value): PaintBot.rotation =>
  switch (Int64.to_int(value)) {
  | 0 => Left
  | 1 => Right
  | _ => raise(Failure("Program output is not a valid rotation"))
  };

let inputPanelColor = (program: Program.t, bot: PaintBot.t): Program.t => {
  let value = ShipHull.panelColor(bot.hull, bot.pos)->colorToValue;
  Program.writeInputValue(program, value);
};

// DAY 11 PART 1

let rec runRobot = (program: Program.t, robot: PaintBot.t) => {
  let program = program->inputPanelColor(robot)->Computer.runUntilBlocked;

  let color = program->Program.readOutputValue->valueToColor;
  let turn = program->Program.readOutputValue->valueToRotation;

  let robot = PaintBot.step(robot, color, turn);

  if (Computer.isHalted(program)) {
    robot;
  } else {
    runRobot(program, robot);
  };
};

let paintedHull = runRobot(paintProgram, robot).hull;
let answer = Map.size(paintedHull);

let test1 = () => assert(answer == 1863);

// DAY 11 PART 2

let paintHullLetters = () => {
  let robot = PaintBot.paintPanel(robot, ShipHull.White);
  runRobot(paintProgram, robot);
};

let paintedHull = paintHullLetters().hull;
let answer = "\n" ++ ShipHull.show(paintedHull) ++ "\n";

let expected = "
...............................................
...............................................
...###..#....#..#.#....####...##.#....####.....
...#..#.#....#..#.#.......#....#.#.......#.....
...###..#....#..#.#......#.....#.#......#......
...#..#.#....#..#.#.....#......#.#.....#.......
...#..#.#....#..#.#....#....#..#.#....#........
...###..####..##..####.####..##..####.####.....
...............................................
...............................................
";

let test2 = () => assert(answer == expected);
