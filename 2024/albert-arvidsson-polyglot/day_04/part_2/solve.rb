#!/usr/bin/env ruby

grid = File.readlines(ARGV[0]).map(&:strip).map(&:chars)
range = 1..(grid.size - 2)
coords = range.to_a.product(range.to_a)
count = 0
get_val = ->((x, y)) { grid.dig(y, x) }

involved = grid.map { |row| row.map { '.' } }
coords.each do |x, y|
  char = grid[y][x]
  next unless char == 'A'

  slash = [[x - 1, y - 1], [x + 1, y + 1]]
  backslash = [[x - 1, y + 1], [x + 1, y - 1]]
  next unless slash.map(&get_val).sort_by(&:to_s) == %w[M S]
  next unless backslash.map(&get_val).sort_by(&:to_s) == %w[M S]

  count += 1

  involved[y][x] = 'A'
  slash.concat(backslash).each do |cx, cy|
    involved[cy][cx] = grid.dig(cy, cx)
  end
end

warn involved.map(&:join).join("\n")

pp count
