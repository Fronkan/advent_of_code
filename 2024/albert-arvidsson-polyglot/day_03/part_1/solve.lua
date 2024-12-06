#!/usr/bin/env lua

local input = assert(io.open(arg[1], "r")):read("*a")
local result = 0
for a, b in input:gmatch("mul%((%d+),(%d+)%)") do
  result = result + tonumber(a) * tonumber(b)
end
print(result)
