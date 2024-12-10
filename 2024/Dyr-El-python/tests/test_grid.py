from utils.grid import Grid2D
from utils.vec import Vec2D


def test_grid_creation():
    inp = """
.X.
...
. .
...""".strip()
    grid = Grid2D(inp)
    assert grid.max_x == 2
    assert grid.max_y == 3


def test_grid_item_access():
    inp = """
.X.
...
. .
...""".strip()
    grid = Grid2D(inp)
    assert grid[(0, 0)] == "."
    assert grid[(1, 0)] == "X"
    assert grid[Vec2D(1, 2)] == " "


def test_grid_coordinates_inside():
    inp = """
.X.
...
. .
...""".strip()
    grid = Grid2D(inp)
    assert (0, 0) in grid
    assert Vec2D(-1, 0) not in grid
    assert (3, 2) not in grid
    assert Vec2D(2, 3) in grid


def test_grid_access_default():
    inp = """
.X.
...
. .
...""".strip()
    grid = Grid2D(inp)
    assert grid.get((0, 0), " ") == "."
    assert grid.get((4, 5), "a") == "a"


def test_grid_iteration():
    inp = """
.X.
...
. .
...""".strip()
    grid = Grid2D(inp)
    i = grid.items()
    assert next(i) == (Vec2D(0, 0), ".")


def test_grid_find():
    inp = """
.X.
...
.X.
...""".strip()
    grid = Grid2D(inp)
    assert grid.find(lambda pos, ch: ch == "X" and pos.y < 2) == [Vec2D(1, 0)]


def test_grid_print():
    inp = """
.X.
..a
. .
...""".strip()
    grid = Grid2D(inp)
    assert str(grid) == inp
