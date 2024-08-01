require 'json'

def get_monkeys
  JSON.parse(File.read("./input.json"), object_class: Monkey)
end

monkey_fields = [
  :id,
  :items,
  :inspect_left,
  :inspect_op,
  :inspect_right,
  :test,
  :if_true,
  :if_false,
  :activity
]

Monkey = Struct.new(*monkey_fields) do
  def inspect_items
    self.activity ||= 0
    self.activity += self.items.length
    self.items = self.items.map do |item|
      left_val =
        if self.inspect_left == "old" then item else self.inspect_left end
      right_val =
        if self.inspect_right == "old" then item else self.inspect_right end

      if self.inspect_op == "*" then
        left_val.monkey_multiply(right_val)
      else
        left_val.monkey_add(right_val)
      end
    end
    self
  end

  def observe_relief(relieved)
    self.items = self.items.map { |item| (item / 3).floor() } if relieved
    self
  end

  def throw_items(monkeys)
    self.items.each do |item|
      if item.modulo(self.test).zero? then
        monkeys[self.if_true].catch_item(item)
      else
        monkeys[self.if_false].catch_item(item)
      end
    end
    self.items = []
    self
  end

  def catch_item(item)
    self.items.unshift(item)
    self
  end
end

class Integer
  @@ring_module_n_size = get_monkeys.map(&:test).inject(:*)

  def monkey_multiply(other)
    (self * other).modulo(@@ring_module_n_size)
  end

  def monkey_add(other)
    (self + other).modulo(@@ring_module_n_size)
  end
end

def round_of_shenanigans(monkeys, relieved:)
  monkeys.map do |monkey|
    monkey.inspect_items.observe_relief(relieved).throw_items(monkeys)
  end
end

def monkey_business(monkeys)
  monkeys.map(&:activity).sort{|a,b| b <=> a}.take(2).inject(:*)
end

def part_1
  monkeys = get_monkeys

  20.times do
    monkeys = round_of_shenanigans(monkeys, relieved: true)
  end

  monkey_business(monkeys)
end

def part_2
  monkeys = get_monkeys

  10_000.times do
    monkeys = round_of_shenanigans(monkeys, relieved: false)
  end

  monkey_business(monkeys)
end

puts part_1
puts part_2
