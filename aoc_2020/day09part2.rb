class CypherSolver
  def initialize(numbers, sum_target)
    @cypher = numbers
    @sum_target = sum_target
    @sums = Array.new(@cypher.length).map { |_| Array.new }
    @sums[0][0] = @cypher[0]
    @encryption_weakness = nil
  end

  def encryption_weakness
    return @encryption_weakness if @encryption_weakness
    range = find_weakness_range
    @encryption_weakness = range.min + range.max
  end

  # Example:
  # cypher = [35, 20, 15, 25, 47, 40, 62, 55]
  # sum_target = 127
  # returns [15, 25, 47, 40] for the range [2..5]
  #
  #    0   1   2   3   4    5
  # 0  35  55  70  95  142
  # 1                  107  147
  # 2                       127
  #
  private def find_weakness_range
    start_index = 0
    end_index = 1
    while start_index < @cypher.length && end_index < @cypher.length
      prev_sum_with_start_index = @sums[start_index][end_index - 1]

      new_sum =
        if prev_sum_with_start_index
          prev_sum_with_start_index + @cypher[end_index]
        else
          @sums[start_index - 1][end_index] - @cypher[start_index - 1]
        end

      @sums[start_index][end_index] = new_sum

      if new_sum == @sum_target
        return @cypher[start_index..end_index]
      elsif new_sum > @sum_target
        start_index = start_index + 1
      else
        end_index = end_index + 1
      end
    end
  end
end

numbers = File.read("puzzle_inputs/day09.txt").split("\n").map { |s| s.to_i }

# input is the solution to part 1
solver = CypherSolver.new(numbers, 20874512)

part_2 = solver.encryption_weakness

puts "Part 2: #{part_2}"

solution = 3012420
raise "Incorrect answer, expected #{solution}" unless part_2 == solution
