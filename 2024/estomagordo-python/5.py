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
    orderings = defaultdict(set)
    updates = []

    for line in lines:
        if not line.strip():
            continue

        if '|' in line:
            a, b = map(int, line.split('|'))
            orderings[a].add(b)
        else:
            updates.append(list(map(int, line.split(','))))

    return orderings, updates
    

def solve_a(lines):
    orderings, updates = parse(lines)

    def score(update):
        for i, num in enumerate(update):
            if num not in orderings:
                continue

            for j in range(i):
                if update[j] in orderings[num]:
                    return 0
                
        return update[len(update)//2]
    
    return sum(score(update) for update in updates)


def solve_b(lines):
    orderings, updates = parse(lines)

    points = 0

    def score(update):
        for i, num in enumerate(update):
            if num not in orderings:
                continue

            for j in range(i):
                if update[j] in orderings[num]:
                    return 0
                
        return update[len(update)//2]
    
    for i, update in enumerate(updates):
        if score(update) > 0:
            continue

        mappings = {val: val for val in update}

        needs_enlarging = True
        while needs_enlarging:
            needs_enlarging = False

            for num, following in orderings.items():
                if num not in mappings:
                    continue

                if needs_enlarging:
                    break

                values = [mappings[f] for f in following if f in mappings]

                if not values:
                    continue

                largest = max(values)

                while mappings[num] < largest:
                    needs_enlarging = True
                    mappings[num] *= num
                    break

        update.sort(key=lambda v: mappings[v])
        points += update[len(update)//2]

    return points


def main():
    lines = []

    with open('5.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())