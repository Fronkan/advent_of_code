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
    return [ints(line) for line in lines]


def all_increasing(report):
        return report == sorted(report)

    
def all_decreasing(report):
    return report == sorted(report, reverse=True)


def diff_okay(pair):
    return abs(pair[0]-pair[1]) in (1, 2, 3)


def all_diff_okay(report):
    return all(diff_okay(pair) for pair in chunks_with_overlap(report, 2))


def safe(report):
    return (all_increasing(report) or  all_decreasing(report)) and all_diff_okay(report)
    

def solve_a(lines):
    reports = parse(lines)
    return sum(safe(report) for report in reports)


def solve_b(lines):
    reports = parse(lines)
    
    def modified_safe(report):
        return safe(report) or any(safe(report[:x] + report[x+1:]) for x in range(len(report)))

    return sum(modified_safe(report) for report in reports)


def main():
    lines = []

    with open('2.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())
