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
    return lines
    

def solve_a(lines):
    val = 0

    for line in lines:
        muls = ''
        a = ''
        b = ''

        for c in line:
            if b or (a and a[-1] == ','):
                if c.isdigit():
                    b += c
                else:
                    if c == ')':
                        val += int(a[:-1]) * int(b)
                    muls = ''
                    a = ''
                    b = ''
            elif a or len(muls) == 4:
                if c.isdigit() or c == ',':
                    a += c
                else:
                    muls = ''
                    a = ''
                    b = ''
            elif muls + c == 'mul('[:len(muls)+1]:
                muls += c
            else:
                muls = ''
                a = ''
                b = ''
                
    return val


def solve_b(lines):
    val = 0
    do = True

    for line in lines:
        muls = ''
        a = ''
        b = ''

        for i, c in enumerate(line):
            if i > 3 and line[i-3:i+1] == 'do()':
                do = True
            if i > 6 and line[i-6:i+1] == "don't()":
                do = False
            if do:
                if b or (a and a[-1] == ','):
                    if c.isdigit():
                        b += c
                    else:
                        if c == ')':
                            val += int(a[:-1]) * int(b)
                        muls = ''
                        a = ''
                        b = ''
                elif a or len(muls) == 4:
                    if c.isdigit() or c == ',':
                        a += c
                    else:
                        muls = ''
                        a = ''
                        b = ''
                elif muls + c == 'mul('[:len(muls)+1]:
                    muls += c
                else:
                    muls = ''
                    a = ''
                    b = ''
                
    return val


def main():
    lines = []

    with open('3.txt') as f:
        for line in f.readlines():
            lines.append(line)
            
    return (solve_a(lines), solve_b(lines))


if __name__ == '__main__':
    print(main())