const assert = require('assert');
const fs = require("fs");

const union = (a, b) => new Set([...a, ...b])
const intersection = (a, b) => new Set([...a].filter(x => b.has(x)));

const countGroups = (input, strat) =>
  input
    .split("\n\n")
    .map(group =>
      group
      .split("\n")
      .map(form => new Set(form))
      .reduce(strat)
      .size)
    .reduce((a, b) => a + b)

puzzleInput = fs.readFileSync(__dirname + "/puzzle_inputs/day06.txt", "utf-8").trim()

const part1 = countGroups(puzzleInput, union)
assert.equal(part1, 6878)
console.log("Part 1:", part1)

const part2 = countGroups(puzzleInput, intersection)
assert.equal(part2, 3464)
console.log("Part 2:", part2)
