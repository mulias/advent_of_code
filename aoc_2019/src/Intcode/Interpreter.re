open Exception;

let addressFromParam =
    ({relativeBase}: Program.t, addressParam: Instruction.addressParam)
    : Memory.address =>
  switch (addressParam) {
  | `AbsoluteAddress(a) => a
  | `RelativeAddress(a) => Memory.Address.add(a, relativeBase)
  };

let paramValue = (state: Program.t, p: Instruction.param): Memory.value =>
  switch (p) {
  | `Value(v) => v
  | `AbsoluteAddress(_) as a
  | `RelativeAddress(_) as a =>
    Memory.read(state.memory, addressFromParam(state, a))
  };

let moveInstructionPointer = (state: Program.t, offset: int): Memory.Address.t =>
  Memory.Address.add(
    state.instructionPointer,
    Memory.Address.of_int(offset),
  );

let instructionValues =
    (state: Program.t)
    : (Memory.value, Memory.value, Memory.value, Memory.value) =>
  Memory.getInstructionValues(state.memory, state.instructionPointer);

let splitOperation = (code: Memory.value): (int, int, int, int) => {
  let paddedCode = Printf.sprintf("%05Ld", code);
  let digits = (start, len) =>
    int_of_string(String.sub(paddedCode, start, len));

  (digits(3, 2), digits(2, 1), digits(1, 1), digits(0, 1));
};

let setParamMode = (value: Memory.value, mode: int): Instruction.param =>
  switch (mode) {
  | 0 => `AbsoluteAddress(value)
  | 1 => `Value(value)
  | 2 => `RelativeAddress(value)
  | _ => raise(InvalidParamMode(mode))
  };

let setAddressParamMode =
    (value: Memory.value, mode: int): Instruction.addressParam =>
  switch (mode) {
  | 0 => `AbsoluteAddress(value)
  | 2 => `RelativeAddress(value)
  | _ => raise(InvalidParamMode(mode))
  };

let readInstruction = (state: Program.t): Instruction.t => {
  let (operation, p1, p2, p3) = instructionValues(state);
  let (opcode, p1Mode, p2Mode, p3Mode) = splitOperation(operation);

  switch (opcode, p1, p2, p3) {
  | (1, read1, read2, write) =>
    Add({
      read1: setParamMode(read1, p1Mode),
      read2: setParamMode(read2, p2Mode),
      write: setAddressParamMode(write, p3Mode),
    })
  | (2, read1, read2, write) =>
    Multiply({
      read1: setParamMode(read1, p1Mode),
      read2: setParamMode(read2, p2Mode),
      write: setAddressParamMode(write, p3Mode),
    })
  | (3, write, _, _) => Input({write: setAddressParamMode(write, p1Mode)})
  | (4, read, _, _) => Output({read: setParamMode(read, p1Mode)})
  | (5, test, setPointer, _) =>
    JumpIfTrue({
      test: setParamMode(test, p1Mode),
      setPointer: setParamMode(setPointer, p2Mode),
    })
  | (6, test, setPointer, _) =>
    JumpIfFalse({
      test: setParamMode(test, p1Mode),
      setPointer: setParamMode(setPointer, p2Mode),
    })
  | (7, x, y, write) =>
    LessThan({
      x: setParamMode(x, p1Mode),
      y: setParamMode(y, p2Mode),
      write: setAddressParamMode(write, p3Mode),
    })
  | (8, x, y, write) =>
    Equals({
      x: setParamMode(x, p1Mode),
      y: setParamMode(y, p2Mode),
      write: setAddressParamMode(write, p3Mode),
    })
  | (9, baseModifier, _, _) =>
    AdjustRelativeBase({baseModifier: setParamMode(baseModifier, p1Mode)})
  | (99, _, _, _) => Halt
  | _ => raise(InvalidOperation(operation, p1, p2, p3))
  };
};

let setMemory =
    (
      state: Program.t,
      addressParam: Instruction.addressParam,
      value: Memory.value,
    )
    : Memory.t => {
  let address = addressFromParam(state, addressParam);
  Memory.write(state.memory, address, value);
};

let applyInstruction =
    (state: Program.t, instruction: Instruction.t): Program.t =>
  switch (instruction) {
  | Add({read1, read2, write}) =>
    let value =
      Memory.Value.add(paramValue(state, read1), paramValue(state, read2));
    let newMemory = setMemory(state, write, value);
    {
      ...state,
      memory: newMemory,
      instructionPointer: moveInstructionPointer(state, 4),
    };
  | Multiply({read1, read2, write}) =>
    let value =
      Memory.Value.mul(paramValue(state, read1), paramValue(state, read2));
    let newMemory = setMemory(state, write, value);
    {
      ...state,
      memory: newMemory,
      instructionPointer: moveInstructionPointer(state, 4),
    };
  | Input({write}) =>
    switch (Program.readInputValue(state)) {
    | exception _ => raise(Exception.InputQueueEmpty)
    | input =>
      let newMemory = setMemory(state, write, input);
      {
        ...state,
        memory: newMemory,
        instructionPointer: moveInstructionPointer(state, 2),
      };
    }
  | Output({read}) =>
    let outVal = paramValue(state, read);
    let _ = Program.writeOutputValue(state, outVal);
    {...state, instructionPointer: moveInstructionPointer(state, 2)};
  | JumpIfTrue({test, setPointer}) =>
    let testVal = paramValue(state, test);
    let newPointer =
      !Memory.Value.isZero(testVal)
        ? paramValue(state, setPointer) : moveInstructionPointer(state, 3);

    {...state, instructionPointer: newPointer};
  | JumpIfFalse({test, setPointer}) =>
    let testVal = paramValue(state, test);
    let newPointer =
      Memory.Value.isZero(testVal)
        ? paramValue(state, setPointer) : moveInstructionPointer(state, 3);

    {...state, instructionPointer: newPointer};
  | LessThan({x, y, write}) =>
    let isLessThan =
      Memory.Value.isLessThan(paramValue(state, x), paramValue(state, y))
        ? Memory.Value.one : Memory.Value.zero;
    let newMemory = setMemory(state, write, isLessThan);
    {
      ...state,
      memory: newMemory,
      instructionPointer: moveInstructionPointer(state, 4),
    };
  | Equals({x, y, write}) =>
    let isEqual =
      Memory.Value.isEqual(paramValue(state, x), paramValue(state, y))
        ? Memory.Value.one : Memory.Value.zero;
    let newMemory = setMemory(state, write, isEqual);
    {
      ...state,
      memory: newMemory,
      instructionPointer: moveInstructionPointer(state, 4),
    };
  | AdjustRelativeBase({baseModifier}) =>
    let modVal = paramValue(state, baseModifier);
    {
      ...state,
      relativeBase: Memory.Address.add(state.relativeBase, modVal),
      instructionPointer: moveInstructionPointer(state, 2),
    };
  | Halt => state
  };
