from utils import parse_lines


def test_simple_lines():
    inp = """1,2
3,4
5,6"""
    expected = [[1, 2], [3, 4], [5, 6]]
    assert parse_lines(inp, lambda line: [int(s) for s in line.split(",")]) == expected


def test_complex_lines():
    inp = """1,2
3,4
5,6

s: h, p, c
r: b, q, ww"""
    expected = [
        [[1, 2], [3, 4], [5, 6]],
        [("s", ["h", "p", "c"]), ("r", ["b", "q", "ww"])],
    ]

    def firstPart(line):
        return [int(s) for s in line.split(",")]

    def secondPart(line):
        key, _, rest = line.partition(": ")
        return (key, [item for item in rest.split(", ")])

    assert parse_lines(inp, [firstPart, secondPart], split_groups="\n\n") == expected
