import importlib
import pytest
from pathlib import Path

DIRECTORY = Path(__file__).parent

solution = importlib.import_module(f"{DIRECTORY.name}.solution")


@pytest.mark.parametrize(
    "puzzle, result",
    [
        (1, 140),
        (2, 80)
    ],
)
def test_puzzle_small_example_input(puzzle, result):
    input_file = Path(__file__).parent / "small_example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result



@pytest.mark.parametrize(
    "puzzle, result",
    [
        (1, 1930),
        (2, 1206)
    ],
)
def test_puzzle_example_input(puzzle, result):
    input_file = Path(__file__).parent / "example_input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result

def test_puzzle_example_input_e():
    input_file = Path(__file__).parent / "example_input_e.txt"
    puzzle_solver = getattr(solution, "puzzle2")
    assert puzzle_solver(input_file) == 236

def test_puzzle_example_input_ab():
    input_file = Path(__file__).parent / "example_input_ab.txt"
    puzzle_solver = getattr(solution, "puzzle2")
    assert puzzle_solver(input_file) == 368


@pytest.mark.parametrize(
    "puzzle, result",
    [
        (1, 772),
        (2, 436)
    ],
)
def test_puzzle_small_example_input_xo(puzzle, result):
    input_file = Path(__file__).parent / "example_input_xo.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result



@pytest.mark.parametrize(
    "puzzle, result",
    [
        (1, 1446042),
        (2, 902742)
    ],
)
def test_puzzle_real_input(puzzle, result):
    input_file = Path(__file__).parent / "input.txt"
    puzzle_solver = getattr(solution, f"puzzle{puzzle}")
    assert puzzle_solver(input_file) == result