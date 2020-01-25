open Utils;
let _logger = (state: Program.t, instruction: Instruction.t) => {
  Js.log(state.instructionPointer);
  Js.log(Instruction.showInstruction(instruction));
};

let program = Program.readProgramFile("src/day05/input.txt");

// DAY 5 PART 1

let answer =
  Program.makeExecutable(program)
  ->Program.writeInputInt(1)
  ->Computer.run
  ->Program.getOutputInts;

let expected = [0, 0, 0, 0, 0, 0, 0, 0, 0, 5577461];

let test1 = () => assert(List.eq(answer, expected, (==)));

// DAY 5 PART 2

let answer =
  Program.makeExecutable(program)
  ->Program.writeInputInt(5)
  ->Computer.run
  ->Program.readOutputInt;

let test2 = () => assert(answer == 7161591);
