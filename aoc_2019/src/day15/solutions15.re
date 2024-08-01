open Utils;
open RepairDroid;

let navProgram =
  Program.readProgramFile("src/day15/input.txt")->Program.makeExecutable;

let parseInput = (input: string): Point.direction =>
  switch (input) {
  | "w" => Up
  | "s" => Down
  | "a" => Left
  | "d" => Right
  | _ => Up
  };

let writeMoveCommand = (program: Program.t, direction: direction): Program.t =>
  Program.writeInputInt(
    program,
    switch (direction) {
    | Up => 1 //    North
    | Down => 2 //  South
    | Left => 3 //  West
    | Right => 4 // East
    },
  );

let sleep = (waitTimeInMs: int) =>
  Js.Promise.make((~resolve, ~reject) => {
    let _ = Js.Global.setTimeout(() => resolve(. true), waitTimeInMs);
    ();
  });

let runDroid = (program: Program.t, droid: t): unit => {
  let events: Queue.t(string) = Queue.create();

  Js.log(show(droid));
  Readline.readline(input => Queue.push(input, events));

  let rec runDroidRec = (program: Program.t, droid: t) => {
    sleep(1000)
    |> Js.Promise.then_(_ =>
         if (Queue.is_empty(events)) {
           runDroidRec(program, droid);
         } else {
           let input = Queue.pop(events);

           let direction = parseInput(input);
           let targetPos = Point.shift(droid.pos, direction);

           let program =
             writeMoveCommand(program, direction)->Computer.runUntilBlocked;
           let moveStatus = Program.readOutputInt(program);

           switch (moveStatus) {
           | 0 =>
             let droid = setTile(droid, targetPos, Wall);
             Js.log(show(droid));
             runDroidRec(program, droid);
           | 1 =>
             let droid = moveDroid(droid, targetPos);
             Js.log(show(droid));
             runDroidRec(program, droid);
           | 2 =>
             Readline.close();
             let droid = moveDroid(droid, targetPos);
             Js.log(show(droid));
             Js.Promise.resolve();
           | i =>
             Readline.close();
             raise(Failure({j|Unexpected movement status $i|j}));
           };
         }
       );
  };

  let _ = runDroidRec(program, droid);
  ();
};

runDroid(navProgram, RepairDroid.start());
