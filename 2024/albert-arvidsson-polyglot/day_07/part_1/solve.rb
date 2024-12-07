#!/usr/bin/env ruby

operators = %i[+ *]
permutate = ->(count) { operators.repeated_permutation(count).to_a }
calibration = lambda do |values|
  permutate.call(values.size - 1).map do |ops|
    values.zip([:+] + ops).reduce(0) do |acc, (val, op)|
      [acc, val].inject(op)
    end
  end
end

pp(File.readlines(ARGV[0]).sum do |line|
  test, *values = line.split(/:? /).map(&:to_i)
  calibration.call(values).include?(test) ? test : 0
end)
