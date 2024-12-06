#!/usr/bin/env ruby

rules, updates = File.read(ARGV[0]).split("\n\n")

rules = rules.split("\n").map { |line| line.split('|').map(&:to_i) }

updates = updates.split("\n").map { |update| update.split(',').map(&:to_i) }

manuals = updates.map do |update|
  sorted = update.sort do |a, b|
    next -1 if rules.find { |(a2, b2)| a2 == a && b2 == b }
    next 1 if rules.find { |(a2, b2)| b2 == a && a2 == b }

    0
  end
  sorted == update ? nil : sorted
end.compact

pp(manuals.sum { |manual| manual[manual.size / 2] })
