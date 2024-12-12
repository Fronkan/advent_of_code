from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque
from functools import cache


def parse(inp: str):
    return [int(s) for s in inp.split()]


@cache
def blink_stone(stone: int, steps: int) -> int:
    """Take the stone inscription and number of blink steps and returns the number of resulting
    stones"""
    if steps == 0:
        return 1
    if stone == 0:
        return blink_stone(1, steps - 1)
    stone_str = str(stone)
    stone_str_half, stone_str_odd = divmod(len(stone_str), 2)
    if not stone_str_odd:
        return (blink_stone(int(stone_str[:stone_str_half]), steps - 1) +  # fmt: skip
                blink_stone(int(stone_str[stone_str_half:]), steps - 1))  #  fmt: skip
    return blink_stone(stone * 2024, steps - 1)


def part1(inp: str) -> int:
    stones = parse(inp)
    return sum(blink_stone(stone, steps=25) for stone in stones)


def part2(inp: str) -> int:
    stones = parse(inp)
    return sum(blink_stone(stone, steps=75) for stone in stones)


ex_inp = """125 17""".strip()


def test_1_1():
    expected = 55312
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 65601038650482
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 11)
    main(prep.get_content())
