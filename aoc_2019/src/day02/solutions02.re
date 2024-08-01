let program = Program.readProgramFile("src/day02/input.txt");

let setInputs = (memory: Memory.t, in1: int, in2: int): Memory.t =>
  Memory.write(memory, Memory.Address.of_int(1), Memory.Value.of_int(in1))
  ->Memory.write(Memory.Address.of_int(2), Memory.Value.of_int(in2));

let calculateOutput = (memory: Memory.t): int =>
  Program.makeExecutable(memory)
  ->Computer.run
  ->Program.memory
  ->Memory.read(Memory.Address.of_int(0))
  ->Memory.Value.to_int;

// DAY 2 PART 1

let answer = setInputs(program, 12, 02)->calculateOutput;

let test1 = () => assert(answer == 4484226);

// DAY 2 PART 2

let findInputs = (program: Memory.t, target: int): (int, int) => {
  let rec findRec = (noun: int, verb: int) => {
    let out = setInputs(program, noun, verb)->calculateOutput;
    if (out == target) {
      (noun, verb);
    } else if (verb > 99) {
      findRec(noun + 1, 0);
    } else {
      findRec(noun, verb + 1);
    };
  };

  findRec(0, 0);
};

let answer = findInputs(program, 19690720);

let test2 = () => assert(answer == (56, 96));
