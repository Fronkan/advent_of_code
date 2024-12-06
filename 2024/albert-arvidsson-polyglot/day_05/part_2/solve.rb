#!/usr/bin/env ruby

rules, updates = File.read(ARGV[0]).split("\n\n").map { |s| s.split("\n") }
rules = rules.to_set

manuals = updates.map { |update| update.split(',') }.map do |update|
  sorted = update.sort do |a, b|
    next -1 if rules.include?("#{a}|#{b}")

    rules.include?("#{b}|#{a}") ? 1 : 0
  end
  sorted == update ? nil : sorted
end.compact

pp(manuals.sum { |manual| manual[manual.size / 2].to_i })
