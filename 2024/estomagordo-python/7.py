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
    equations = {}

    for line in lines:
        parts = line.split(':')
        answer = int(parts[0])
        nums = list(map(int, parts[1].split()))
        equations[answer] = nums

    return equations
    

def solve_a(lines):
    equations = parse(lines)

    def can_be_satisfied(answer, equation):
        n = len(equation)

        def works(val, pos):
            if pos == n:
                return val == answer
            
            return works(val*equation[pos], pos+1) or works(val+equation[pos], pos+1)
        
        return works(equation[0], 1)

    return sum(answer for answer, equation in equations.items() if can_be_satisfied(answer, equation))


def solve_b(lines):
    equations = parse(lines)

    def can_be_satisfied(answer, equation):
        n = len(equation)

        def works(val, pos):
            if pos == n:
                return val == answer
            
            return works(val*equation[pos], pos+1) or works(val+equation[pos], pos+1) or works(int(str(val) + str(equation[pos])), pos+1)
        
        return works(equation[0], 1)

    return sum(answer for answer, equation in equations.items() if can_be_satisfied(answer, equation))


def main():
    lines = []

    with open('7.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())