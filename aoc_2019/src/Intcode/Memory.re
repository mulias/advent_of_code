open Belt;

module Value = {
  include Int64;

  exception ConversionDataLoss(t);

  let isEqual = (x: t, y: t): bool => compare(x, y) == 0;

  let isZero = (value: t): bool => isEqual(value, zero);

  let isLessThan = (x: t, y: t): bool => compare(x, y) < 0;

  let isGreaterThan = (x: t, y: t): bool => compare(x, y) > 0;

  let toIntExn = (v: t): int => {
    let maxInt32 = of_int32(Int32.max_int);
    if (isGreaterThan(v, maxInt32)) {
      raise(ConversionDataLoss(v));
    } else {
      to_int(v);
    };
  };
};

module ValueComparable =
  Id.MakeComparable({
    type t = Value.t;
    let cmp = Value.compare;
  });

module Address = Value;

type value = Value.t;

type address = Address.t;

type t = Map.t(address, value, ValueComparable.identity);

let init = () => Map.make(~id=(module ValueComparable));

let write = (memory: t, address: address, value: value): t =>
  Map.set(memory, address, value);

let fromList = (l: list(value)): t =>
  List.mapWithIndex(l, (idx, value) => (Value.of_int(idx), value))
  ->List.reduce(init(), (memory: t, (address, value)) =>
      write(memory, address, value)
    );

let read = (memory: t, address: address): value =>
  Map.get(memory, address)->Option.getWithDefault(Value.of_int(0));

let getInstructionValues =
    (memory: t, opAddress: address): (value, value, value, value) => {
  let offset = (i: int) => Value.add(opAddress, Value.of_int(i));

  (
    read(memory, offset(0)),
    read(memory, offset(1)),
    read(memory, offset(2)),
    read(memory, offset(3)),
  );
};
