from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque
from functools import cache, reduce
from itertools import combinations


DIRECTIONS = {"<":Vec2D(-1, 0), "^": Vec2D(0, -1), ">": Vec2D(1, 0), "v": Vec2D(0, 1)}


def parse_single(inp: str) -> tuple[Grid2D, str]:
    grid_lines, instruction_lines = parse_lines(inp, [lambda x:x, lambda x: x], "\n\n")
    grid = Grid2D('\n'.join(grid_lines))
    instructions = ''.join(instruction_lines)
    return grid, instructions


def parse_double(inp: str) -> tuple[Grid2D, str]:
    grid_lines, instruction_lines = parse_lines(inp, [lambda x:x, lambda x: x], "\n\n")
    double_lines = []
    for line in grid_lines:
        double_line = []
        for c in line:
            double_line.append({"#":"##", "O":"[]", ".": "..", "@": "@."}[c])
        double_lines.append(''.join(double_line))
    grid = Grid2D('\n'.join(double_lines))
    instructions = ''.join(instruction_lines)
    return grid, instructions


def doMoveSingle(grid: Grid2D, pos: Vec2D, i: str) -> bool:
    if grid[pos] in "O@":
        if doMoveSingle(grid, DIRECTIONS[i] + pos, i):
            grid[DIRECTIONS[i] + pos] = grid[pos]
            grid[pos] = "."
            return True
    elif grid[pos] == ".":
        return True
    return False


def checkMoveDouble(grid: Grid2D, pos: Vec2D, i: str) -> bool:
    if grid[pos] == "@":
        return checkMoveDouble(grid, DIRECTIONS[i] + pos, i)
    elif grid[pos] in "[":
        if i in "<>":
            return checkMoveDouble(grid, DIRECTIONS[i] + pos, i)
        else:
            return (checkMoveDouble(grid, DIRECTIONS[i] + pos, i) and 
                    checkMoveDouble(grid, DIRECTIONS[i] + pos + Vec2D(1, 0), i))
    elif grid[pos] in "]":
        if i in "<>":
            return checkMoveDouble(grid, DIRECTIONS[i] + pos, i)
        else:
            return (checkMoveDouble(grid, DIRECTIONS[i] + pos, i) and 
                    checkMoveDouble(grid, DIRECTIONS[i] + pos + Vec2D(-1, 0), i))
    elif grid[pos] == ".":
        return True
    return False

def performMoveDouble(grid: Grid2D, pos: Vec2D, i: str):
    if grid[pos] == "@":
        performMoveDouble(grid, DIRECTIONS[i] + pos, i)
        grid[DIRECTIONS[i] + pos] = grid[pos]
        grid[pos] = '.'
    elif grid[pos] in "[":
        if i in "<>":
            performMoveDouble(grid, DIRECTIONS[i] + pos, i)
            grid[DIRECTIONS[i] + pos] = grid[pos]
            grid[pos] = '.'
        else:
            performMoveDouble(grid, DIRECTIONS[i] + pos, i)
            performMoveDouble(grid, DIRECTIONS[i] + pos + Vec2D(1, 0), i)
            grid[DIRECTIONS[i] + pos] = grid[pos]
            grid[pos] = '.'
            grid[DIRECTIONS[i] + pos + Vec2D(1, 0)] = grid[pos + Vec2D(1, 0)]
            grid[pos + Vec2D(1, 0)] = '.'
    elif grid[pos] in "]":
        if i in "<>":
            performMoveDouble(grid, DIRECTIONS[i] + pos, i)
            grid[DIRECTIONS[i] + pos] = grid[pos]
            grid[pos] = '.'
        else:
            performMoveDouble(grid, DIRECTIONS[i] + pos, i)
            performMoveDouble(grid, DIRECTIONS[i] + pos + Vec2D(-1, 0), i)
            grid[DIRECTIONS[i] + pos] = grid[pos]
            grid[pos] = '.'
            grid[DIRECTIONS[i] + pos + Vec2D(-1, 0)] = grid[pos + Vec2D(-1, 0)]
            grid[pos + Vec2D(-1, 0)] = '.'


def doMoveDouble(grid: Grid2D, pos: Vec2D, i: str) -> bool:
    if checkMoveDouble(grid, pos, i):
        performMoveDouble(grid, pos, i)
        return True
    return False


def score(grid: Grid2D) -> int:
    return sum((pos.x + pos.y * 100)
               for pos in grid.find(lambda _, x: x in 'O['))



def part1(inp: str) -> int:
    grid, instructions = parse_single(inp)
    pos = grid.find(lambda _, ch: ch=="@")[0]
    for instruction in instructions:
        if doMoveSingle(grid, pos, instruction):
            pos = DIRECTIONS[instruction] + pos
    return score(grid)


def part2(inp: str) -> int:
    grid, instructions = parse_double(inp)
    pos = grid.find(lambda _, ch: ch=="@")[0]
    for instruction in instructions:
        if doMoveDouble(grid, pos, instruction):
            pos = DIRECTIONS[instruction] + pos
    return score(grid)


ex_inp = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".strip()

def test_1_1():
    expected = 10092
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 9021
    assert part2(ex_inp) == expected

def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 15)
    main(prep.get_content())
