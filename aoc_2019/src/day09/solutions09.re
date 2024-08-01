let program = Program.readProgramFile("src/day09/input.txt");

let _logger = (state: Program.t, instruction: Instruction.t) => {
  Js.log(Int64.to_string(state.instructionPointer));
  Js.log(Instruction.showInstruction(instruction));
};

// DAY 9 PART 1

let answer =
  Program.makeExecutable(program)
  ->Program.writeInputInt(1)
  ->Computer.run
  ->Program.readOutputValue
  ->Memory.Value.to_string;

let test1 = () => assert(answer == "2465411646");

// DAY 9 PART 2

let answer =
  Program.makeExecutable(program)
  ->Program.writeInputInt(2)
  ->Computer.run
  ->Program.readOutputInt;

let test2 = () => assert(answer == 69781);
