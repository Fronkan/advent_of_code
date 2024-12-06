from aoc_prepare import PrepareAoc
from utils import Vec2D, Grid2D, parse_lines


def parse1(line: str):
    x, _, y = line.partition("|")
    return int(x), int(y)


def parse2(line: str):
    return [int(x) for x in line.split(",")]


def parse(inp):
    return parse_lines(inp, [parse1, parse2], split_groups="\n\n")


def right_order(x, y, order):
    if x not in order:
        return True
    if order[x] == y:
        return True
    return right_order(order[x])


def correct_order(update, order):
    return not any(
        (y, x) in order for xidx, x in enumerate(update) for y in update[xidx + 1 :]
    )


def fix_order(update, before):
    update = update[:]
    for xidx in range(len(update) - 1):
        for yidx in range(xidx + 1, len(update)):
            x, y = update[xidx], update[yidx]
            if (y, x) in before:
                update[xidx], update[yidx] = update[yidx], update[xidx]
    return update


def part1(inp):
    order, updates = parse(inp)
    before = {(x, y) for x, y in order}
    return sum(
        (
            update[len(update) // 2]
            for update in updates
            if correct_order(update, before)
        )
    )


def part2(inp):
    order, updates = parse(inp)
    before = {(x, y) for x, y in order}
    return sum(
        (fix_order(update, before)[len(update) // 2])
        for update in updates
        if not correct_order(update, before)
    )


def test_1_1():
    assert (
        part1(
            """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
        )
        == 143
    )


def test_1_2():
    assert (
        part2(
            """47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"""
        )
        == 123
    )


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 5)
    main(prep.get_content())
