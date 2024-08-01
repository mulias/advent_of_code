open Printf;

type valueParam = [ | `Value(Memory.value)];
type absoluteAddressParam = [ | `AbsoluteAddress(Memory.address)];
type relativeAddressParam = [ | `RelativeAddress(Memory.address)];
type addressParam = [ absoluteAddressParam | relativeAddressParam];
type param = [ valueParam | absoluteAddressParam | relativeAddressParam];

type t =
  | Add({
      read1: param,
      read2: param,
      write: addressParam,
    })
  | Multiply({
      read1: param,
      read2: param,
      write: addressParam,
    })
  | Input({write: addressParam})
  | Output({read: param})
  | JumpIfTrue({
      test: param,
      setPointer: param,
    })
  | JumpIfFalse({
      test: param,
      setPointer: param,
    })
  | LessThan({
      x: param,
      y: param,
      write: addressParam,
    })
  | Equals({
      x: param,
      y: param,
      write: addressParam,
    })
  | AdjustRelativeBase({baseModifier: param})
  | Halt;

let showValueParam = (`Value(v): valueParam): string =>
  sprintf("Value(%Ld)", v);

let showAddressParam = (addressParam: addressParam): string =>
  switch (addressParam) {
  | `AbsoluteAddress(a) => sprintf("AbsoluteAddress(%Ld)", a)
  | `RelativeAddress(a) => sprintf("RelativeAddress(%Ld)", a)
  };

let showParam = (param: param): string =>
  switch (param) {
  | `Value(_) as v => showValueParam(v)
  | `AbsoluteAddress(_) as a
  | `RelativeAddress(_) as a => showAddressParam(a)
  };

let showInstruction = (instruction: t): string =>
  switch (instruction) {
  | Add({read1, read2, write}) =>
    sprintf(
      "Add({read1: %s, read2: %s, write: %s})",
      showParam(read1),
      showParam(read2),
      showAddressParam(write),
    )
  | Multiply({read1, read2, write}) =>
    sprintf(
      "Multiply({read1: %s, read2: %s, write: %s})",
      showParam(read1),
      showParam(read2),
      showAddressParam(write),
    )
  | Input({write}) => sprintf("Input({write: %s})", showAddressParam(write))
  | Output({read}) => sprintf("Output({read: %s})", showParam(read))
  | JumpIfTrue({test, setPointer}) =>
    sprintf(
      "JumpIfTrue({test: %s, setPointer: %s})",
      showParam(test),
      showParam(setPointer),
    )
  | JumpIfFalse({test, setPointer}) =>
    sprintf(
      "JumpIfFalse({test: %s, setPointer: %s})",
      showParam(test),
      showParam(setPointer),
    )
  | LessThan({x, y, write}) =>
    sprintf(
      "LessThan({x: %s, y: %s, write: %s})",
      showParam(x),
      showParam(y),
      showAddressParam(write),
    )
  | Equals({x, y, write}) =>
    sprintf(
      "Equals({x: %s, y: %s, write: %s})",
      showParam(x),
      showParam(y),
      showAddressParam(write),
    )
  | AdjustRelativeBase({baseModifier}) =>
    sprintf(
      "AdjustRelativeBase({baseModifier: %s})",
      showParam(baseModifier),
    )
  | Halt => "Halt"
  };

type varient = [
  | `Add
  | `Multiply
  | `Input
  | `Output
  | `JumpIfTrue
  | `JumpIfFalse
  | `LessThan
  | `Equals
  | `AdjustRelativeBase
  | `Halt
];

let toVarient = (instruction: t): varient =>
  switch (instruction) {
  | Add(_) => `Add
  | Multiply(_) => `Multiply
  | Input(_) => `Input
  | Output(_) => `Output
  | JumpIfTrue(_) => `JumpIfTrue
  | JumpIfFalse(_) => `JumpIfFalse
  | LessThan(_) => `LessThan
  | Equals(_) => `Equals
  | AdjustRelativeBase(_) => `AdjustRelativeBase
  | Halt => `Halt
  };

let is = (varient: varient, instruction: t): bool =>
  toVarient(instruction) == varient;
