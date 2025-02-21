from argparse import ArgumentParser
from functools import cache
from pathlib import Path
from typing import Iterable


def puzzle1(input_file: Path):
    stones = list(_read_data(input_file))
    return sum(blink_stone(stone, 25) for stone in stones)


@cache
def blink_stone(stone: int, rounds: int) -> int:
    
    # When we reach rounds == 0 we have no more expansions to do 
    # and hence only a single stone
    if rounds == 0:
        return 1

    # Otherwise we might still expand further depending on the number
    if stone == 0:
        return blink_stone(1, rounds - 1)
    elif len(stone_str := str(stone)) % 2 == 0:
        split = len(stone_str) // 2
        return blink_stone(int(stone_str[:split]), rounds - 1) + blink_stone(
            int(stone_str[split:]), rounds - 1
        )
    else:
        return blink_stone(stone * 2024, rounds - 1)


def puzzle2(input_file: Path):
    stones = list(_read_data(input_file))
    return sum(blink_stone(stone, 75) for stone in stones)


def _read_data(input_file: Path) -> Iterable[int]:
    yield from (int(v.strip()) for v in input_file.read_text().split())


if __name__ == "__main__":
    print("Day 11")

    parser = ArgumentParser()
    parser.add_argument("--example", action="store_true", default=False)
    args = parser.parse_args()

    if args.example:
        print("running example file")
        input_file = Path(__file__).parent / "example_input.txt"
    else:
        input_file = Path(__file__).parent / "input.txt"
    try:
        print("Puzzle 1:", puzzle1(input_file))
    except NotImplementedError:
        print("puzzle 1 not implemented yet")
    try:
        print("Puzzle 2:", puzzle2(input_file))
    except NotImplementedError:
        print("puzzle 2 not implemented yet")
