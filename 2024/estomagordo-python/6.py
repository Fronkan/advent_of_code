from bisect import bisect_left, bisect_right
from collections import Counter, defaultdict, deque
from functools import cache, reduce
from heapq import heapify, heappop, heappush
from itertools import combinations, permutations, product
from math import ceil, comb, factorial, gcd, isclose, lcm

from algo import a_star, custsort, merge_ranges, sssp
from constants import EPSILON, HUGE, UNHUGE
from helpers import adjacent, between, chunks, chunks_with_overlap, columns, digits, dimensions, distance, distance_sq, eight_neighs, eight_neighs_bounded, forward_rays_with_diagonals, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded, overlap, positives, rays, rays_from_inside, words


def parse(lines):
    return [list(line.rstrip()) for line in lines]
    

def solve_a(lines):
    turns = '<^>v'
    direction = {
        '<': (0, -1),
        '>': (0, 1),
        'v': (1, 0),
        '^': (-1, 0)
    }

    grid = parse(lines)
    height = len(grid)
    width = len(grid[0])

    gy = -1
    gx = -1
    gd = -1

    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c in '<>v^':
                gy = y
                gx = x
                
                for i, cc in enumerate(turns):
                    if cc == c:
                        gd = i
    
    seen = {(gy, gx)}
    
    while True:
        d = direction[turns[gd]]
        ngy = gy+d[0]
        ngx = gx+d[1]

        if ngy == -1 or ngx == -1 or ngy == height or ngx == width:
            break

        if grid[ngy][ngx] == '#':
            gd = (gd+1) % 4
            continue

        gy = ngy
        gx = ngx

        seen.add((gy, gx))


    return len(seen)


def solve_b(lines):
    turns = '<^>v'
    direction = {
        '<': (0, -1),
        '>': (0, 1),
        'v': (1, 0),
        '^': (-1, 0)
    }

    grid = parse(lines)
    height = len(grid)
    width = len(grid[0])

    sy = -1
    sx = -1
    sd = -1

    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c in '<>v^':
                sy = y
                sx = x
                
                for i, cc in enumerate(turns):
                    if cc == c:
                        sd = i
    
    seen = {(sy, sx)}
    gy = sy
    gx = sx
    gd = sd
    
    while True:
        d = direction[turns[gd]]
        ngy = gy+d[0]
        ngx = gx+d[1]

        if ngy == -1 or ngx == -1 or ngy == height or ngx == width:
            break

        if grid[ngy][ngx] == '#':
            gd = (gd+1) % 4
            continue

        gy = ngy
        gx = ngx

        seen.add((gy, gx))

    def add_obstruction(y, x):
        lgrid = [list(row) for row in grid]
        lgrid[y][x] = '#'

        gy = sy
        gx = sx
        gd = sd
        lseen = {(sy, sx, sd)}

        while True:
            d = direction[turns[gd]]
            ngy = gy+d[0]
            ngx = gx+d[1]

            if ngy == -1 or ngx == -1 or ngy == height or ngx == width:
                return False

            if lgrid[ngy][ngx] == '#':
                gd = (gd+1) % 4
            else:
                gy = ngy
                gx = ngx

            if (gy, gx, gd) in lseen:
                return True

            lseen.add((gy, gx, gd))

    return sum(add_obstruction(y, x) for y, x in seen if (y, x) != (sy, sx))


def main():
    lines = []

    with open('6.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())

# 5209 wrong A