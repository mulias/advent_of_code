require 'matrix'

def cave_map
  File.read("puzzle_inputs/day15.txt")
    .split("\n")
    .map { |row| row.split('').map(&:to_i) }
    .then { |rows| Matrix[*rows] }
end

def pos_risk(map, x, y)
  if x.negative? || y.negative? then nil else map[x,y] end
end

def path_risk(map, x, y)
  min_prev_risk = [
     pos_risk(map, x - 1, y),
     pos_risk(map, x, y - 1),
   ].reject(&:nil?).min || 0

  min_prev_risk + pos_risk(map, x, y)
end

def part_1
  map = cave_map()

  map.each_with_index do |_, x, y|
    map[x,y] = path_risk(map, x, y)
  end

  map[-1, -1] - map[0, 0]
end
