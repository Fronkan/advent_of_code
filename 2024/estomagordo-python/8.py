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
    grid = parse(lines)
    height, width = dimensions(grid)

    antennae = defaultdict(list)

    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c == '.':
                continue

            antennae[c].append((y, x))

    antinodes = set()

    for pairs in antennae.values():
        for a, b in combinations(pairs, 2):
            diffy = b[0]-a[0]
            diffx = b[1]-a[1]

            candidates = ((a[0]-diffy, a[1]-diffx), (b[0]+diffy, b[1]+diffx))

            for cy, cx in candidates:
                if 0 <= cy < height and 0 <= cx < width:
                    antinodes.add((cy, cx))

    return len(antinodes)


def solve_b(lines):
    grid = parse(lines)
    height, width = dimensions(grid)

    antennae = defaultdict(list)

    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c == '.':
                continue

            antennae[c].append((y, x))

    antinodes = set()

    for pairs in antennae.values():
        for a, b in combinations(pairs, 2):
            diffy = b[0]-a[0]
            diffx = b[1]-a[1]
            g = gcd(diffy, diffx)

            backstep = 0

            while 0 <= a[0]-diffy//g*backstep < height and 0 <= a[1]-diffx//g*backstep < width:
                antinodes.add((a[0]-diffy//g*backstep, a[1]-diffx//g*backstep))
                backstep += 1

            forwstep = 1

            while 0 <= a[0]+diffy//g*forwstep < height and 0 <= a[1]+diffx//g*forwstep < width:
                antinodes.add((a[0]+diffy//g*forwstep, a[1]+diffx//g*forwstep))
                forwstep += 1

    return len(antinodes)

  
def main():
    lines = []

    with open('8.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())
