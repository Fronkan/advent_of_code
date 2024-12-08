from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from itertools import product
from functools import reduce
import operator


def parse_line(line):
    first, _, second = line.partition(":")
    l = [int(s) for s in second.strip().split()]
    return [int(first), l]


def parse(inp):
    result = parse_lines(inp, parse_line)
    return result


def find_eq(value, eq, funcs):
    for operators in product(funcs, repeat=(len(eq) - 1)):
        tot = eq[0]
        for op, arg in zip(operators, eq[1:]):
            tot = op(tot, arg)
        if tot == value:
            return value
    return 0


def part1(inp):
    eqs = parse(inp)
    return sum((find_eq(eq[0], eq[1], (operator.add, operator.mul)) for eq in eqs))


def part2(inp):
    eqs = parse(inp)
    return sum(
        (
            find_eq(
                eq[0],
                eq[1],
                (operator.add, operator.mul, lambda x, y: int(str(x) + str(y))),
            )
            for eq in eqs
        )
    )


ex_inp = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""".strip()


def test_1_1():
    expected = 3749
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 11387
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 7)
    main(prep.get_content())
