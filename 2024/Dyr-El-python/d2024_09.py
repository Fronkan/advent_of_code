from aoc_prepare import PrepareAoc
from utils import parse_lines, Grid2D, Vec2D
from collections import namedtuple, deque


FileItem = namedtuple("FileItem", ['id', 'position', 'length'])
SpaceItem = namedtuple("SpaceItem", ['position', 'length'])


def parse(inp: str)-> tuple[deque[FileItem], deque[SpaceItem]]:
    files = deque()
    space = deque()
    pos = 0
    for idx, c in enumerate(inp):
        c = int(c)
        if idx % 2 == 0:
            files.append(FileItem(id=(idx // 2), position=pos, length=c))
        else:
            space.append(SpaceItem(position=pos, length=c))
        pos += c
    return files, space


def tick_block(files: deque[FileItem], idx: int) -> int:
    file = files[idx]
    files[idx] = FileItem(id=file.id, position=file.position, length=(file.length - 1))
    return file.id


def part1(inp: str):
    files, spaces = parse(inp)
    block_position = 0
    total = 0
    while files:
        while files[0].length > 0:
            total += (tick_block(files, 0) * block_position)
            block_position += 1
        files.popleft()
        if not files:
            return total
        while spaces[0].length != 0:
            if files[-1].length > 0:
                total += (tick_block(files, -1) * block_position)
                block_position += 1
            else:
                files.pop()
                continue
            spaces[0] = SpaceItem(position=spaces[0].position, length=(spaces[0].length - 1))
        spaces.popleft()
    return block_position


def find_free_space(spaces: list[SpaceItem], file: FileItem):
    for space_idx, space in enumerate(spaces):
        if space.position > file.position:
            break
        if space.length >= file.length:
            return space.position, space_idx
    return file.position, None


def part2(inp):
    files, spaces = parse(inp)
    blocks = dict()
    for file in reversed(files):
        new_position, space_to_use = find_free_space(spaces, file)
        for block_idx in range(new_position, new_position + file.length):
            blocks[block_idx] = file.id
        if space_to_use is not None:
            spaces[space_to_use] = SpaceItem(position=(spaces[space_to_use].position + file.length), 
                                            length=spaces[space_to_use].length - file.length)
    return sum((block_position * file_id for block_position, file_id in blocks.items()))



ex_inp = """2333133121414131402""".strip()


def test_1_1():
    expected = 1928
    assert part1(ex_inp) == expected


def test_1_2():
    expected = 2858
    assert part2(ex_inp) == expected


def main(inp):
    print("Part1:", part1(inp.strip()))
    print("Part2:", part2(inp.strip()))


if __name__ == "__main__":
    prep = PrepareAoc(2024, 9)
    main(prep.get_content())
