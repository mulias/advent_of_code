# Advent of Code 2019, The Intcode Problems

Every Year [Advent of Code](https://adventofcode.com/events) releases a holiday
themed series of programming puzzles, unlocked in small bundles from December
1st through 25th. While I've been aware of the challenge in the past, this was
my first year digging in to the puzzles in earnest.

For [Advent of Code 2019](https://adventofcode.com/2019) I focused on solving
the series of related puzzles featuring the fictional "Intcode Computer" used to
operate Santa's interplanetary sleigh. The Intcode interpreter is introduced on
day 2 of the calendar, and then used for increasingly more complex tasks on all
odd numbered days from day 5 through 25.

Since the core of these puzzles involves building and using an interpreter for
an esoteric programming language, it seemed natural to use OCaml, a language
with a [solid](https://github.com/andrejbauer/plzoo)
[history](https://www.cs.yale.edu/flint/cs421/case-for-ml.html) in PL and
compilers. As a twist, I've been meaning to try out
[ReasonML](https://reasonml.github.io/) for a while, so I chose to give Reason's
syntax and tooling a shot.

My Intcode interpreter and puzzle solutions are [available on
github](https://github.com/mulias/advent-of-code-2019).


### What I learned

Thoughts on OCaml, ReasonML, and AoC19, in no particular order:

* OCaml's module system is powerful and lends itself well to modeling
  a problem domain. I saw this proved out over and over, but one example that
  stood out to me was on [day 9](https://adventofcode.com/2019/day/9) which adds
  the requirement "The computer should have support for large numbers." Up until
  that point I had performed all operations on `int`s, but to meet this new
  requirement I had to refactor everything to `int64`. I was able to encapsulate
  the extra complexity and refactor cleanly by creating the
  `Intcode.Memory.Value` and `Intcode.Memory.Address` modules.

* The first time I used OCaml I had a lot of trouble understanding how to use
  Polymorphic Variants. Now that I'm revisiting the language with the added
  context of structurally typed languages such as Typescript, Polymorphic
  Variants seem much more natural. In the `Intcode.Instruction` module I use
  this language feature to define the `Value`, `AbsoluteAddress`, and
  `RelativeAddress` variants, which represent the three ways an Intcode integer
  can be interpreted. By using appropriate combinations of these types we can
  accurately encode interpreter rules into the types. It's a light splash of
  more flexible structural typing into a nominally typed language.

* In an early version of my Intcode interpreter I used `Result` types to track
  every Intcode operation that could fail, threading through the computation
  state and returning a final value or error. While this technique works well
  for many real-world use cases, I decided that in the constrained world of
  solving puzzles with well-defined inputs it was too much overhead. Errors
  would only ever trigger if there was a bug in my code, so failing fast with
  an exception proved more valuable than rigorously accounting for every case
  with `Result`s.

* ReasonML's standard library leaves a lot to be desired. I ended up going with
  [Belt](https://bucklescript.github.io/bucklescript/api/Belt.html), which is
  also far from perfect. It's still unclear to me why I can't use a well
  established OCaml utils library such as [core](https://github.com/janestreet/core),
  but I didn't really research the issue.

* I also noticed a mismatch between data-first libraries that use the
  fast-pipe operator `->`, and data-last libraries that use the `|>` pipe
  operator. This wasn't much more than an inconvenience, but it seems like an
  unnecessary pain point for new users. I ended up using `->`, which comes with
  some kind of macro for designating the curried value with an underscore, e.g.
  `str->Js.String.split(",", _)`.

* In OCaml [`ppx_deriving`](https://github.com/ocaml-ppx/ppx_deriving) is used
  for "type-driven code generation." I don't have a lot of experience with this
  tooling, but it's particularly useful for things like `[@@deriving show]`
  which generates functions to pretty-print complex data structures.
  BuckleScript has a little bit of this functionality built in with features
  like `[@bs.deriving accessors]`, but it's pretty limited. The
  [`bs-deriving`](https://github.com/ELLIOTTCABLE/bs-deriving) library claims to
  have ported `ppx_deriving` to BuckleScript, but it uses pre-compiled binaries
  that I couldn't get working on my system. It would be nice if more `deriving`
  features were folded into BuckleScript, or even introduced at the Reason
  level.

* The puzzles in this series can be split between Intcode implementation
  problems and application problems. I liked this, since it meant that I had to
  think about both the internal organization of the interpreter, and the
  external "Intcode Computer" API. You can see this play out for problems such
  as [day 7](https://adventofcode.com/2019/day/7), where 5 separate "computers"
  perform calculations in series.

* Advent of code is very time consuming! Each day is a 2-part puzzle, so doing
  the full calendar means solving 50 moderately-involved programming puzzles.

### Progress and Roadblocks

Day 13 is complete, day 15 is unfinished. I lost steam on this project shortly
after New Year's, both because I ran out of free time and because day 15 throws
in a new twist which I've found to be frustrating more because of language
limitations than because of inherent complexity.

From a code complexity perspective [day 15](https://adventofcode.com/2019/day/15)
could be solved by either over-engineering the repair droid as an autonomous unit
that explores its surroundings, or by simply allowing a user to input
directional commands to navigate to the goal. Unfortunately implementing the
simple solution in the terminal with Reason and the underlying node runtime is
proving difficult. Node's callback style doesn't mesh well with Reason, and I
haven't found a way to get the program to block execution while waiting for user
input. It's a mess!

If I were to revisit this project I might try to build a simple
[ReasonReact](https://reasonml.github.io/reason-react/) app to run the Intcode
program and process user input in the browser. This solution would potentially
unlock many of the subsequent puzzles for me, since user input plays a large
part in the later Intcode puzzles.
