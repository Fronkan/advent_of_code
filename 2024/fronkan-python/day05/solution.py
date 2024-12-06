from argparse import ArgumentParser
from collections import defaultdict
from pathlib import Path

from aoc_lib.input_readers import read_chunks

def puzzle1(input_file: Path):
    rules, updates = read_chunks(input_file)
    dependencies = defaultdict(list)
    for rule in rules:
        v1, v2 = (int(v) for v in rule.split("|"))
        dependencies[v1].append(v2)

    count = 0
    for update_str in updates:
        update = [int(v) for v in update_str.split(",")]
        if _is_sorted(update, dependencies):
            count += update[len(update)//2]
    return count

def _is_sorted(update, dependencies):
    visited_pages = set()
    for page in update:
        for dependency in dependencies[page]:
            if dependency in visited_pages:
                return False
        visited_pages.add(page)
    return True


def puzzle2(input_file: Path):
    rules, updates = read_chunks(input_file)
    dependencies = defaultdict(list)
    for rule in rules:
        v1, v2 = (int(v) for v in rule.split("|"))
        dependencies[v1].append(v2)

    count = 0
    for update_str in updates:
        update = [int(v) for v in update_str.split(",")]
        if not _is_sorted(update, dependencies):
            update = _sort_update(update, dependencies)
            count += update[len(update)//2]
    return count

def _sort_update(update, dependencies):
    pages = []
    for page in update:
        broken_dependencies = [
            dependency
            for dependency in dependencies[page]
            if dependency in pages
        ] 
        if broken_dependencies:
            lowest_index = min(pages.index(dep) for dep in broken_dependencies)
            pages.insert(lowest_index, page)
        else:
            pages.append(page)

    return pages


if __name__ == "__main__":
    print("Day 5")

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