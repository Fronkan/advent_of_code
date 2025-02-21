from argparse import ArgumentParser
from collections import Counter
from pathlib import Path

def puzzle1(input_file: Path):
    values = [int(v) for v in input_file.read_text().splitlines()[0]]
    end_index = len(values)//2
    out = []
    forward_stream = stream_indices(values, 0, 1)
    reverse_stream = (idx for idx in stream_indices(reversed(values), end_index, -1) if idx is not None)
    forward_idx_count = Counter()
    backward_idx_count = Counter()
    while True:
        idx = next(forward_stream)
        if idx in backward_idx_count:
            # Make sure we don't duplicate things that has been moved
            forward_idx_count[idx] += 1
            while next(forward_stream) == idx:
                forward_idx_count[idx] += 1
            for _ in range(forward_idx_count[idx] - backward_idx_count[idx]):
                out.append(idx)
            break
        elif idx is not None:
            out.append(idx)
            forward_idx_count[idx] += 1
        else:
            ridx = next(reverse_stream)
            backward_idx_count[ridx] +=1
            out.append(ridx)

    return sum(idx*val for idx, val in enumerate(out))

def stream_indices(values, start_idx, step):
    file_idx = start_idx
    for comp_idx, val in enumerate(values):
        if comp_idx%2 == 0:
            for _ in range(val):
                yield file_idx
            file_idx += step
        else:
            for _ in range(val):
                yield None


def puzzle2(input_file: Path):
    values = [int(v) for v in input_file.read_text().splitlines()[0]]
    blocks = [
        (
            (pos//2) if pos%2 == 0 else None,
            block_size
        )
        for pos, block_size in enumerate(values)
    ]
    end_pos = len(blocks) -1
    # idx_stream = [idx if idx is not None else "." for idx, block_size in blocks for _ in range(block_size)]
    # print(*idx_stream)
    while end_pos >= 0:
        block_idx, block_size = blocks[end_pos]
        if block_idx is None:
            end_pos -= 1
            continue
        for pos, empty_block in _iter_empty(blocks):
            if empty_block[1] >= block_size and pos < end_pos:
                blocks[pos] = (block_idx, block_size)
                blocks[end_pos] = (None, block_size)
                if (remaining_space := empty_block[1] - block_size):
                    blocks.insert(pos+1, (None, remaining_space))
                break
        # idx_stream = [idx if idx is not None else "." for idx, block_size in blocks for _ in range(block_size)]
        # print(*idx_stream)
        end_pos -= 1



    idx_stream = [idx for idx, block_size in blocks for _ in range(block_size)]
    # print(blocks)
    # print(*idx_stream)

    return sum(
        pos * idx
        for pos, idx in enumerate(idx_stream)
        if idx is not None
    )

def _iter_empty(blocks):
    for pos, block in enumerate(blocks):
        if block[0] is None:
            yield pos, block

if __name__ == "__main__":
    print("Day 9")

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