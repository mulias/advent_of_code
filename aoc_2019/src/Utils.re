open Belt;

module Option = {
  include Belt.Option;

  let andThen = (o: option('a), f: 'a => option('b)): option('b) =>
    switch (o) {
    | Some(a) => f(a)
    | None => None
    };
};

module List = {
  include Belt.List;

  let set = (l: list('a), nth: int, a: 'a): option(list('a)) =>
    switch (splitAt(l, nth)) {
    | Some((before, [_, ...after])) => Some(before @ [a] @ after)
    | Some((_before, [])) => None
    | None => None
    };

  let flatMap = (l: list('a), f: 'a => list('b)): list('b) =>
    flatten(map(l, f));

  let takeUpTo = (l: list('a), n): list('a) =>
    take(l, n)->Option.getWithDefault(l);

  let diff = (x: list(int), y: list(int)): list(int) => {
    let xSet = Set.Int.fromArray(List.toArray(x));
    let ySet = Set.Int.fromArray(List.toArray(y));
    Set.Int.toList(Set.Int.diff(xSet, ySet));
  };

  let extractIndex = (l: list('a), idx: int): option(('a, list('a))) =>
    switch (splitAt(l, idx)) {
    | Some((front, [a, ...back])) => Some((a, front @ back))
    | Some((_front, [])) => None
    | None => None
    };

  let permutations = (elements: list(int)): list(list(int)) => {
    let rec permutationsRec = acc => {
      flatMap(acc, perm => {
        switch (diff(elements, perm)) {
        | [] => [perm]
        | remaining => permutationsRec(map(remaining, add(perm)))
        }
      });
    };

    permutationsRec([[]]);
  };

  let range = (s: int, e: int): list(int) => fromArray(Array.range(s, e));

  let joinWith = (l: list(string), j: string): string =>
    Js.Array.joinWith(j, List.toArray(l));
};

module Result = {
  include Belt.Result;

  let andThen =
      (r: result('a, 'b), f: 'a => result('c, 'b)): result('c, 'b) =>
    switch (r) {
    | Ok(a) => f(a)
    | Error(b) => Error(b)
    };

  let map2 =
      (r1: result('a, 'e), r2: result('b, 'e), f: ('a, 'b) => 'c)
      : result('c, 'e) =>
    switch (r1, r2) {
    | (Ok(a), Ok(b)) => Ok(f(a, b))
    | (Error(e), _) => Error(e)
    | (_, Error(e)) => Error(e)
    };
};

module Queue = {
  include Queue;

  let toList = (q: t('a)) => fold(List.add, [], q)->List.reverse;

  let fromList = (l: list('a)): t('a) => {
    let queue = create();
    List.forEach(l, a => add(a, queue));
    queue;
  };
};

module Map = {
  include Map;

  let keysToList = (m: t('k, 'v, 'id)): list('k) =>
    List.fromArray(keysToArray(m));
};

module String = {
  include String;

  let explode = (s: string): list(char) =>
    List.range(0, length(s))->List.map(get(s));
};
