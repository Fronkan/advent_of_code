from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque
from functools import cache, reduce
from itertools import combinations

def parse_line(inp):
    if inp == "":
        return None
    part1, _, part2 = inp.partition(": ")
    cx, cy = part2.split(", ")
    if part1.startswith("Button "):
        part1 = part1[-1]
    cx, cy = int(cx[2:]), int(cy[2:])
    return part1, cx, cy


def parse(inp):
    segms = parse_lines(inp, parse_line)
    machines = list()
    machine = dict()
    for segm in segms:
        if segm is None:
            if machine:
                machines.append(machine)
            machine = dict()
            continue
        machine[segm[0]] = segm[1:]
    machines.append(machine)
    return machines
        

def part1(inp):
    machines = parse(inp)
    tot = 0
    for machine in machines:
        x, y = machine["Prize"]
        ax, ay = machine["A"] 
        bx, by = machine["B"] 
        mincost = x * 5
        for a in range(x//ax + 1):
            remx, remy = x - ax*a, y - ay*a
            if remx % bx == 0 and remy % by == 0 and remx//bx == remy//by:
                cost = a*3 + remx//bx
                if cost < mincost:
                    mincost = cost
        if mincost == x * 5:
            mincost = 0
        tot += mincost
    return tot

from math import gcd, sqrt
def primes(n):
    for i in range(2, int(sqrt(n))):
        if n % i == 0:
            return [i] + primes(n // i)
    return [n]

def part2(inp):
    machines = parse(inp)
    tot = 0
    for machine in machines:
        x_p, y_p = machine["Prize"]
        x_p, y_p = x_p + 10000000000000, y_p + 10000000000000
        x_a, y_a = machine["A"] 
        x_b, y_b = machine["B"] 
        t_b = round((x_p - y_p / y_a * x_a) / (x_b - y_b * x_a / y_a))
        t_a = round((y_p - (x_p - y_p / y_a * x_a) / (x_b - y_b * x_a / y_a) * y_b) / y_a)
        try:
            assert x_p == t_a * x_a + t_b * x_b
            assert y_p == t_a * y_a + t_b * y_b
        except AssertionError:
            continue
        tot += t_a * 3 + t_b
    return tot


ex_inp = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279""".strip()


def test_1_1():
    expected = 480
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 875318608908
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 13)
    main(prep.get_content())
