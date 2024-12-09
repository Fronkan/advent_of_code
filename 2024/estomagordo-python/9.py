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
    return list(map(int, lines[0]))
    

def solve_a(lines):
    data = parse(lines)

    file_count = 0
    file_system = []

    for i, n in enumerate(data):
        for _ in range(n):
            file_system.append(-1 if i%2 else file_count)
        if i%2 == 0:
            file_count += 1

    n = len(file_system)

    head_pointer = 0
    tail_pointer = n-1

    while True:
        while file_system[head_pointer] != -1:
            head_pointer += 1

        while file_system[tail_pointer] == -1:
            tail_pointer -= 1

        if head_pointer > tail_pointer:
            break

        file_system[head_pointer], file_system[tail_pointer] = file_system[tail_pointer], file_system[head_pointer]

    return sum(i * c for i, c in enumerate(file_system) if c > -1)


def solve_b(lines):
    data = parse(lines)

    file_count = 0
    file_system = []

    for i, n in enumerate(data):
        for _ in range(n):
            file_system.append(-1 if i%2 else file_count)
        if i%2 == 0:
            file_count += 1

    n = len(file_system)

    tail_pointer = n-1

    while file_system[tail_pointer] == -1:
        tail_pointer -= 1

    while tail_pointer >= 0:
        file_val = file_system[tail_pointer]
        file_length = 1

        while tail_pointer > 0 and file_system[tail_pointer-1] == file_val:
            tail_pointer -= 1
            file_length += 1

        head_pointer = 0

        while head_pointer+file_length <= tail_pointer and any(file_system[head_pointer+x] > -1 for x in range(file_length)):
            head_pointer += 1

        if head_pointer+file_length <= tail_pointer:
            for x in range(file_length):
                file_system[head_pointer+x], file_system[tail_pointer+x] = file_system[tail_pointer+x], file_system[head_pointer+x]

        while tail_pointer >= 0 and file_system[tail_pointer] in (-1, file_val):
            tail_pointer -= 1

    return sum(i * c for i, c in enumerate(file_system) if c > -1)


def main():
    lines = []

    with open('9.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())
