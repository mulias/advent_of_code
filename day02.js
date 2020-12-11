const assert = require('assert');
const fs = require("fs");

fs.readFile(__dirname + "/puzzle_inputs/day02.txt", 'utf8', (_error, data) => {
  passwordEntryRegex = /^(?<firstNum>\d+)-(?<secondNum>\d+) (?<policyChar>[a-z]): (?<password>[a-z]+)$/

  passwordEntries = data.trim().split('\n')
    .map(line => line.match(passwordEntryRegex).groups)

  part1 = valid_passwords_with_range_requirement(passwordEntries).length
  assert.equal(part1, 586)
  console.log("Part 1:", part1)

  part2 = valid_passwords_with_position_requirement(passwordEntries).length
  assert.equal(part2, 352)
  console.log("Part 2:", part2)
});

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
