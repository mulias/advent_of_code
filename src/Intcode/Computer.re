open Program;

let rec run = (state: t): t => {
  let instruction = Interpreter.readInstruction(state);
  Program.log(state, instruction);

  switch (instruction) {
  | Halt => state
  | _ => run(Interpreter.applyInstruction(state, instruction))
  };
};

let rec runUntilBlocked = (state: t): t => {
  let instruction = Interpreter.readInstruction(state);
  Program.log(state, instruction);

  switch (instruction) {
  | Halt => state
  | Input(_) when !Program.hasInput(state) => state
  | _ => runUntilBlocked(Interpreter.applyInstruction(state, instruction))
  };
};

let isHalted = (state: t): bool =>
  Instruction.is(`Halt, Interpreter.readInstruction(state));
