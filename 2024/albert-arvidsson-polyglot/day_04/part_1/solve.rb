#!/usr/bin/env ruby

word = 'XMAS'
grid = File.readlines(ARGV[0]).map(&:strip).map(&:chars)
range = 0...grid.size
dirs = [[-1, -1], [0, -1], [1, -1], [-1, 0], [1, 0], [-1, 1], [0, 1], [1, 1]]
count = 0

involved = grid.map { |row| row.map { '.' } }

grid.map.with_index do |row, y|
  row.map.with_index do |char, x|
    next unless char == word[0]

    dirs.each do |dx, dy|
      nx = x
      ny = y
      current = [[nx, ny]]
      next unless word[1..].each_char.all? do |expected|
        nx += dx
        ny += dy
        current << [nx, ny]
        letter = grid.dig(ny, nx)
        letter == expected && range.cover?(nx) && range.cover?(ny)
      end

      current.each do |cx, cy|
        involved[cy][cx] = grid.dig(cy, cx)
      end
      count += 1
    end
  end
end

warn involved.map(&:join).join("\n")

pp count
