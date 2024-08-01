open Utils;

[@bs.deriving accessors]
type t = {
  id: string,
  memory: Memory.t,
  instructionPointer: Memory.address,
  relativeBase: Memory.address,
  input: Queue.t(Memory.value),
  output: Queue.t(Memory.value),
  logger: option((t, Instruction.t) => unit),
};

let makeExecutable = (~id=?, ~input=?, ~output=?, ~logger=?, program): t => {
  id: Option.getWithDefault(id, "Program"),
  memory: program,
  instructionPointer: Memory.Address.zero,
  relativeBase: Memory.Address.zero,
  input: Option.getWithDefault(input, Queue.create()),
  output: Option.getWithDefault(output, Queue.create()),
  logger,
};

let readProgramFile = (file: string): Memory.t =>
  Node.Fs.readFileAsUtf8Sync(file)
  ->Js.String.trim
  ->Js.String.split(",", _)
  ->List.fromArray
  ->List.map(Memory.Value.of_string)
  ->Memory.fromList;

let readInputValue = (state: t): Memory.value => Queue.pop(state.input);

let readInputInt = (state: t): int =>
  readInputValue(state)->Memory.Value.toIntExn;

let readOutputValue = (state: t): Memory.value => Queue.pop(state.output);

let readOutputInt = (state: t): int =>
  readOutputValue(state)->Memory.Value.toIntExn;

let writeInputValue = (state: t, value: Memory.value): t => {
  Queue.push(value, state.input);
  state;
};

let writeInputInt = (state: t, i: int): t =>
  Memory.Value.of_int(i)->writeInputValue(state, _);

let writeOutputValue = (state: t, value: Memory.value): t => {
  Queue.push(value, state.output);
  state;
};

let writeOutputInt = (state: t, i: int): t =>
  Memory.Value.of_int(i)->writeOutputValue(state, _);

let hasInput = (state: t): bool => !Queue.is_empty(state.input);

let hasOutput = (state: t): bool => !Queue.is_empty(state.output);

let getInputValues = (state: t): list(Memory.value) =>
  Queue.toList(state.input);

let getInputInts = (state: t): list(int) =>
  getInputValues(state)->List.map(Memory.Value.toIntExn);

let getOutputValues = (state: t): list(Memory.value) =>
  Queue.toList(state.output);

let getOutputInts = (state: t): list(int) =>
  getOutputValues(state)->List.map(Memory.Value.toIntExn);

let log = (state: t, instruction: Instruction.t): unit =>
  Option.forEach(state.logger, logger => logger(state, instruction));
