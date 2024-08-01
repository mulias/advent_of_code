# Constructs an array describing the breeding habits of a single lantern fish.
# Each cell `d` of the array defines how many total fish there will be on
# `last_day` if the starting fish's internal timer first hits 0 on day `d`.
#
# Example:
#
#  day         0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18  19..inf
#  total fish  7  7  5  5  4  4  4  4  4  3  3  2  2  2  2  2  2  2  1     1
#
# This table tells us that a fish with a starting timer of 1 will make 6 more
# fish, for a total of 7 fish on day 18. We calculate this value via:
#
#   timeline[1] = 1 + timeline[10] + timeline[17] + timeline[24]
#               = 1 +      3       +      2       +      1
#
# The 1 represents the fish itself, and the index values represent the number
# of children and grandchildren originating on day 2 (which reaches timer 0 on
# day 10), day 9 (timer 0 on day 17), and day 16 (timer 0 on day 24).
#
# By filling in the timeline from back to front we can calculate the number of
# fish on `last_day` in linear time, bound by either the number of days or
# number of starting fish, whichever is larger.
def fish_creation_timeline(last_day)
  timeline = []

  for day in (0..last_day).to_a.reverse
    timeline[day] =
      (day + 1)
      .then { |first_new_fish_day| first_new_fish_day..last_day }
      .step(7)
      .map{ |day| timeline[day + 8] || 1 }
      .inject(1, :+)
  end

  timeline
end

def fish_on_day(d)
  fish = File.read("puzzle_inputs/day06.txt").split(",").map { |s| s.to_i }
  timeline = fish_creation_timeline(d)

  fish.map { |f| timeline[f] }.inject(&:+)
end

puts "Part 1: #{fish_on_day(80)}"
puts "Part 2: #{fish_on_day(256)}"

raise "Part 1 error" unless fish_on_day(80) == 350149
raise "Part 2 error" unless fish_on_day(256) == 1590327954513
