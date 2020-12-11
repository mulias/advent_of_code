# Advent of Code 2020

https://adventofcode.com/2020

### Running the puzzle solutions

Each `dayXX` program will:

- Read the puzzle input from the `puzzle_inputs` directory.
- Print the solutions to part 1 and part 2 of that day's puzzle.
- Run tests to check that the found answer matches a hard-coded
  solution which has been confirmed by Advent of Code.

The nix-shell environment `nix/default.nix` contains all the tools needed to run
the solutions. I have ran everything in the shell using these commands:

```
day 01: runhaskell day01.hs
day 02: node day02.js
day 03: ruby day03.rb
day 04: runhaskell day04.hs
day 05: raco pkg install threading; racket day05.rkt
day 06: node day06.js
day 07: swipl -q -f day07.pl
day 08: swipl -q -f day08.pl
```
