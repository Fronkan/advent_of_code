from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque
from functools import cache, reduce
from itertools import combinations


def parse_line(inp):
    p, _, v = inp.partition(" ")
    p = Vec2D(*eval(f"({p[2:]})"))
    v = Vec2D(*eval(f"({v[2:]})"))
    return p, v


def parse(inp):
    return parse_lines(inp, parse_line)


def count_robots(robots, minx, miny, maxx, maxy):
    return sum((minx <= robot[0].x <= maxx and miny <= robot[0].y <= maxy) 
               for robot in robots)


def plot(robots, maxx, maxy):
    lines = []
    for y in range(maxy+1):
        line = []
        for x in range(maxx+1):
            line.append({True:"X", False:"."}[sum((robot[0].as_tuple() == (x, y) 
                                                   for robot in robots)) > 0])
        lines.append(''.join(line))
    return '\n'.join(lines)


def calc_safety(robots, max_x, max_y):
    next_robots = []
    for robot in robots:
        next_robots.append(((robot[0] + robot[1] * 100).clamp(0, 0, max_x, max_y), robot[1]))
    robots = next_robots
    max_x2 = max_x // 2 - 1
    max_y2 = max_y // 2 - 1
    return (count_robots(robots, 0, 0, max_x2, max_y2) *
            count_robots(robots, max_x2 + 2, 0, max_x, max_y2) *
            count_robots(robots, max_x2 + 2, max_y2 + 2, max_x, max_y) *
            count_robots(robots, 0, max_y2 + 2, max_x2, max_y))


def calc_context(robots):
    tot = 0
    av_x = sum((r1[0].x for r1 in robots)) // len(robots)
    return sum((100 / (abs(r1[0].x - av_x) + 1) for r1 in robots))


def find_convergence(robots, max_x, max_y):
    context = []
    idx = 0
    while True:
        context.append((calc_context(robots), idx))
        if len(context) > 20:
            ordered_contexts = sorted(context)
            if (ordered_contexts[-1][0] - ordered_contexts[-2][0] == ordered_contexts[-2][0] - ordered_contexts[-3][0] and
                ordered_contexts[-2][0] - ordered_contexts[-3][0] == ordered_contexts[-3][0] - ordered_contexts[-4][0]):
                period = abs(ordered_contexts[-1][1] - ordered_contexts[-2][1])
                offset = idx % period
                return period, offset
        next_robots = []
        for robot in robots:
            next_robots.append(((robot[0] + robot[1]).clamp(0, 0, max_x, max_y), robot[1]))
        robots = next_robots
        idx += 1
    

def find_pattern(robots, max_x, max_y):
    period, offset = find_convergence(robots, max_x, max_y)
    next_robots = []
    for robot in robots:
        next_robots.append(((robot[0] + robot[1] * offset).clamp(0, 0, max_x, max_y), robot[1]))
    robots = next_robots
    count = offset
    while True:
        print(".", end="", flush=True)
        count = count + period
        next_robots = []
        for robot in robots:
            next_robots.append(((robot[0] + robot[1] * period).clamp(0, 0, max_x, max_y), robot[1]))
        robots = next_robots
        s = plot(robots, max_x, max_y)
        if "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" in s:
            print()
            return count


def part1(inp, maxx, maxy):
    robots = parse(inp)
    return calc_safety(robots, maxx, maxy)


def part2(inp, maxx, maxy):
    robots = parse(inp)
    return find_pattern(robots, maxx, maxy)


ex_inp = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3""".strip()


def test_1_1():
    expected = 12
    assert part1(ex_inp, 10, 6) == expected


def main(inp):
    print("Part1:", part1(inp.strip(), 100, 102))
    print("Part2:", part2(inp.strip(), 100, 102))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 14)
    main(prep.get_content())
