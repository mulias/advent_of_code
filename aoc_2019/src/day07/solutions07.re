open Utils;

type ampPhaseSettings = (int, int, int, int, int);

let program = Program.readProgramFile("src/day07/input.txt");

let listToFiveTuple = (l: list('a)): ('a, 'a, 'a, 'a, 'a) =>
  switch (l) {
  | [a, b, c, d, e] => (a, b, c, d, e)
  | _ => raise(Failure("not a five touple"))
  };

let _logger =
    ({id, instructionPointer, input, output}: Program.t, instruction) => {
  let inputArray = Queue.toList(input)->List.toArray;
  let outputArray = Queue.toList(output)->List.toArray;

  Js.log({j|Program $id at position $instructionPointer|j});
  Js.log(Instruction.showInstruction(instruction));
  Js.log({j|input: $inputArray|j});
  Js.log({j|output: $outputArray|j});
};

// DAY 7 PART 1

let phaseSettingsPermutations =
  List.permutations([0, 1, 2, 3, 4])->List.map(listToFiveTuple);

let runSeriesAmp = (phaseSetting: int, input: int): int => {
  Program.makeExecutable(program)
  ->Program.writeInputInt(phaseSetting)
  ->Program.writeInputInt(input)
  ->Computer.run
  ->Program.readOutputInt;
};

let amplifySignal = ((p0, p1, p2, p3, p4): ampPhaseSettings): int => {
  runSeriesAmp(p0, 0) //    Amp A
  ->runSeriesAmp(p1, _) //  Amp B
  ->runSeriesAmp(p2, _) //  Amp C
  ->runSeriesAmp(p3, _) //  Amp D
  ->runSeriesAmp(p4, _); // Amp E
};

let answer =
  List.map(phaseSettingsPermutations, amplifySignal)
  ->List.sort((a, b) => b - a)
  ->List.headExn;

let test1 = () => assert(answer == 46014);

// DAY 7 PART 2

let phaseSettingsPermutations =
  List.permutations([5, 6, 7, 8, 9])->List.map(listToFiveTuple);

let makeFeedbackAmp =
    (
      name,
      ~phase: int,
      ~input: Queue.t(Memory.value),
      ~output: Queue.t(Memory.value),
    ) => {
  Program.makeExecutable(~id=name, ~input, ~output, program)
  ->Program.writeInputInt(phase);
};

let amplifySignal = ((p0, p1, p2, p3, p4): ampPhaseSettings): int => {
  let aToB = Queue.create();
  let bToC = Queue.create();
  let cToD = Queue.create();
  let dToE = Queue.create();
  let eToA = Queue.create();

  let ampA =
    makeFeedbackAmp("A", ~phase=p0, ~input=eToA, ~output=aToB)
    ->Program.writeInputInt(0);
  let ampB = makeFeedbackAmp("B", ~phase=p1, ~input=aToB, ~output=bToC);
  let ampC = makeFeedbackAmp("C", ~phase=p2, ~input=bToC, ~output=cToD);
  let ampD = makeFeedbackAmp("D", ~phase=p3, ~input=cToD, ~output=dToE);
  let ampE = makeFeedbackAmp("E", ~phase=p4, ~input=dToE, ~output=eToA);

  let rec runFeedbackAmps = (a, b, c, d, e) => {
    let ra = Computer.runUntilBlocked(a);
    let rb = Computer.runUntilBlocked(b);
    let rc = Computer.runUntilBlocked(c);
    let rd = Computer.runUntilBlocked(d);
    let re = Computer.runUntilBlocked(e);

    if (Computer.isHalted(re)) {
      Program.readOutputInt(re);
    } else {
      runFeedbackAmps(ra, rb, rc, rd, re);
    };
  };

  runFeedbackAmps(ampA, ampB, ampC, ampD, ampE);
};

let answer =
  List.map(phaseSettingsPermutations, amplifySignal)
  ->List.sort((a, b) => b - a)
  ->List.headExn;

let test2 = () => assert(answer == 19581200);
