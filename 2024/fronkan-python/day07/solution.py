from argparse import ArgumentParser
from pathlib import Path
import operator as op


def puzzle1(input_file: Path):
    total = 0
    for line in input_file.read_text().splitlines():
        tv_str, inputs = line.split(":")
        tv = int(tv_str)
        vals = [int(i) for i in inputs.split(" ") if i]
        if _combine(0, vals, tv, ops=[op.mul, op.add]):
            total += tv
    return total


def _combine(acc, vals, test, ops):
    if not vals:
        return acc == test
    if acc > test:
        return False
    return any([_combine(op(acc, vals[0]), vals[1:], test, ops) for op in ops])


def puzzle2(input_file: Path):
    total = 0
    for line in input_file.read_text().splitlines():
        tv_str, inputs = line.split(":")
        tv = int(tv_str)
        vals = [int(i) for i in inputs.split(" ") if i]
        if _combine(0, vals, tv, ops=[op.mul, op.add, lambda v1, v2: int(f"{v1}{v2}")]):
            total += tv
    return total


if __name__ == "__main__":
    print("Day 7")

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
