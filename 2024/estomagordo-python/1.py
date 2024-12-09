from bisect import bisect_left, bisect_right
from collections import Counter, defaultdict, deque
from functools import cache, reduce
from heapq import heapify, heappop, heappush
from itertools import combinations, permutations, product
from math import ceil, comb, factorial, gcd, isclose, lcm

from algo import a_star, custsort, merge_ranges, sssp
from constants import EPSILON, HUGE, UNHUGE
from helpers import adjacent, between, chunks, chunks_with_overlap, columns, digits, dimensions, distance, distance_sq, eight_neighs, eight_neighs_bounded, grouped_lines, ints, manhattan, multall, n_neighs, neighs, neighs_bounded, overlap, positives, rays, rays_from_inside, words


def parse(lines):
    left = []
    right = []

    for line in lines:
        a, b = map(int, line.split())
        left.append(a)
        right.append(b)

    return left, right
    

def solve_a(lines):
    left, right = parse(lines)

    left.sort()
    right.sort()

    return sum(abs(left[i]-right[i]) for i in range(len(left)))


def solve_b(lines):
    left, right = parse(lines)
    right_counts = Counter(right)

    return sum(num * right_counts[num] for num in left)


def main():
    lines = []

    with open('1.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())
