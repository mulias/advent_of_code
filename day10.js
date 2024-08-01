const assert = require('assert');
const fs = require("fs");

const getJoltageRatings = () => {
  puzzleInput = fs.readFileSync(__dirname + "/puzzle_inputs/day10.txt", "utf-8").trim()
  adapterRatings = puzzleInput.split('\n').map(s => Number(s)).sort((a,b) => a - b)
  deviceRating = Math.max(...adapterRatings) + 3
  return [0, ...adapterRatings, deviceRating]
}

const joltageRatings = getJoltageRatings()

const joltageDifference = (ratings, idx) =>
  ratings[idx - 1] != null && ratings[idx] != null && ratings[idx] - ratings[idx - 1]

const countDifferences = (ratings, difference) =>
  ratings.filter((_n, idx) => joltageDifference(ratings, idx) == difference).length

const splitSubRanges = (ratings) =>
  ratings.reduce((acc, n, idx) => {
    const diff = joltageDifference(ratings, idx) 
    if (diff && diff < 3) {
      let range = acc.pop()
      range.push(n)
      acc.push(range)
    } else {
      acc.push([n])
    }
    return acc
  }, [])

const countSubRangeArrangments = (subRange) =>
  subRange.length < 3 ? 1 : Math.pow(2, subRange.length - 2)

const countAdapterArrangments = (ratings) => {
  const subRanges = splitSubRanges(ratings)
  return subRanges.map(countSubRangeArrangments).reduce((acc, count) => acc * count, 1)
}

const part1 = countDifferences(joltageRatings, 1) * countDifferences(joltageRatings, 3)
assert.equal(part1, 1917)
console.log("Part 1:", part1)

console.log(countAdapterArrangments(joltageRatings))
