from argparse import ArgumentParser
from pathlib import Path
import re

from aoc_lib.input_readers import read_chunks

import numpy as np


XY_MATCH = re.compile(r"([0-9]+)")

COSTS = np.array([3, 1])


def puzzle1(input_file: Path):
    total = 0
    for chunk in read_chunks(input_file):
        A = np.array([int(v) for v in XY_MATCH.findall(chunk[0])])
        B = np.array([int(v) for v in XY_MATCH.findall(chunk[1])])
        prize = np.array([int(v) for v in XY_MATCH.findall(chunk[2])])
        res = solve(A, B, prize)
        if res is not None:
            total += res
    return total


def solve(A, B, prize, epsilon=0.0000000001):
    counts  = np.linalg.solve(np.array([A, B]).transpose(), prize)
    counts_int = counts.round()
    # check if the the integer solution actually works
    if ((counts_int*np.array([A, B]).transpose()).sum(axis=1) == prize).all():
        return int(np.sum(counts_int * COSTS).round())
    return None

def puzzle2(input_file: Path):
    total = 0
    for chunk in read_chunks(input_file):
        A = np.array([int(v) for v in XY_MATCH.findall(chunk[0])])
        B = np.array([int(v) for v in XY_MATCH.findall(chunk[1])])
        prize = np.array([int(v) for v in XY_MATCH.findall(chunk[2])]) + 10000000000000
        print(A,B, prize)
        res = solve(A, B, prize)
        if res is not None:
            total += res
    return total


# too low: 1545093008499
# too high: 157632809244058
# too high: 157632809244414
# Wroing: 156842082522858


if __name__ == "__main__":
    print("Day 13")

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
