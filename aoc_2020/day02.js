const assert = require('assert');
const fs = require("fs");

const puzzleInput = fs.readFileSync(__dirname + "/puzzle_inputs/day02.txt", "utf-8").trim().split('\n')

const passwordEntryRegex = /^(?<firstNum>\d+)-(?<secondNum>\d+) (?<policyChar>[a-z]): (?<password>[a-z]+)$/

const passwordEntries = puzzleInput.map(line => line.match(passwordEntryRegex).groups)

const valid_passwords_with_range_requirement = (passwordEntries) =>
  passwordEntries.map(entry => {
    const {firstNum, secondNum, policyChar, password} = entry
    const count = password.split('').filter(c => c == policyChar).length
    return firstNum <= count && count <= secondNum
  }).filter(isTrue => isTrue)

const valid_passwords_with_position_requirement = (passwordEntries) =>
  passwordEntries.map(entry => {
    const {firstNum, secondNum, policyChar, password} = entry
    const firstCharMatch = password[firstNum - 1] == policyChar
    const secondCharMatch = password[secondNum - 1] == policyChar
    return (firstCharMatch && !secondCharMatch) || (!firstCharMatch && secondCharMatch)
  }).filter(isTrue => isTrue)

const part1 = valid_passwords_with_range_requirement(passwordEntries).length
assert.equal(part1, 586)
console.log("Part 1:", part1)

const part2 = valid_passwords_with_position_requirement(passwordEntries).length
assert.equal(part2, 352)
console.log("Part 2:", part2)
