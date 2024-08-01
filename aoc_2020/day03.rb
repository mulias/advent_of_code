class Map
  attr_reader :height, :width

  def initialize(map_source, map_marks = nil)
    @map_source = map_source
    @map_grid = map_source.split("\n").map { |row| row.split("") }
    @map_marks = map_marks || init_map_marks
    @height = @map_grid.length
    @width = @map_grid[0].length
  end

  def read(x, y)
    if @map_grid[x][y] == "#" then :tree else :open end
  end

  def mark
    new_marks = @map_marks.clone.map(&:clone)
    yield(new_marks)
    Map.new(@map_source, new_marks)
  end

  def to_s
    @map_grid.map.with_index { |row, row_idx|
      row.map.with_index { |map_at_coord, col_idx|
        mark_at_coord = @map_marks[row_idx][col_idx]
        mark_at_coord || map_at_coord
      }.join("")
    }.join("\n")
  end

  private def init_map_marks
    @map_grid.map { |row| row.map { |_| nil } }
  end
end

Step = Struct.new(:x, :y, :at_coord)

class Path
  def initialize(map, down:, right:)
    @map = map
    @steps = traverse(map, down, right)
  end

  def trees_hit
    @steps.select {|step| step.at_coord == :tree}.length
  end

  def to_s
    marked_map = @map.mark { |marks|
      @steps.each { |step|
        marks[step.x][step.y] =
          if step.at_coord == :tree then "X" else "O" end
      }
    }
    marked_map.to_s
  end

  private def traverse(map, down_strat, right_strat)
    total_steps = (map.height.to_f / down_strat).ceil

    (0...total_steps).to_a.map { |step|
      x = step * down_strat
      y = (step * right_strat) % map.width
      at_coord = map.read(x, y)
      Step.new(x, y, at_coord)
    }
  end
end

puzzle_input = File.read("puzzle_inputs/day03.txt")
my_map = Map.new(puzzle_input)

# 3.1

path = Path.new(my_map, right: 3, down: 1)
part_1 = path.trees_hit

puts path
puts
puts "Part 1"
puts "Trees encountered: #{part_1}"

solution = 211
raise "Incorrect answer, expected #{solution}" unless part_1 == solution

# 3.2

part_2 = [
  {right: 1, down: 1},
  {right: 3, down: 1},
  {right: 5, down: 1},
  {right: 7, down: 1},
  {right: 1, down: 2}
].map { |strat|
  Path.new(my_map, strat).trees_hit
}.inject(:*)

puts
puts "Part 2"
puts "Product of trees encountered: #{part_2}"

solution = 3584591857
raise "Incorrect answer, expected #{solution}" unless part_2 == solution
