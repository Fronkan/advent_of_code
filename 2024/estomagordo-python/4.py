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
    height = len(grid)
    width = len(grid[0])

    def find(y, x, target):
        reversed = ''.join(target[::-1])
        n = len(target)

        def test(word):
            return word in (target, reversed)

        return sum(test(''.join(ray[:n])) for ray in forward_rays_with_diagonals(grid, y, x))

    return sum(find(y, x, 'XMAS') for y, x in product(range(height), range(width)))


def solve_b(lines):
    grid = parse(lines)
    height = len(grid)
    width = len(grid[0])
    count = 0

    for y in range(height-2):
        for x in range(width-2):
            if grid[y+1][x+1] != 'A':
                continue

            if {grid[y][x], grid[y+2][x+2]} == set('MS') and {grid[y][x+2], grid[y+2][x]} == set('MS'):
                count += 1

    return count


def main():
    lines = []

    with open('4.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())
