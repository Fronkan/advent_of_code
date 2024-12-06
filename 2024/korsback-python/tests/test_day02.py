import pytest
from days import day02

given_test_data = [
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9",
]

prepped_data = [
    [7,6,4,2,1],
    [1,2,7,8,9],
    [9,7,6,2,1],
    [1,3,2,4,5],
    [8,6,4,4,1], 
    [1,3,6,7,9]
]


def test_get_prepped_lines():
    assert day02.get_prepped_lines(given_test_data) == prepped_data

@pytest.mark.parametrize(
    "data, expected",
    [
        ([1,2,3,4,5], True),
        ([1,2,3,4,9], False),
        ([9,7,5,2,1], True),
        ([9,7,5,7,5], False),
        ([9,7,5,5,3], False),
    ]
)
def test_get_is_safe(data, expected):
    assert day02.get_is_safe(data) == expected

def test_get_direction():
    assert day02.get_direction(1,8) == True
    assert day02.get_direction(8,1) == False

def test_run_a():
    assert day02.run_a(prepped_data) == 2

@pytest.mark.parametrize(
    "data, expected",
    [
        ([1,2,3,4,5], True),
        ([9,7,5,5,3], True),
        ([9,7,8,5,1], False),
        ([50,40,47,45,33], False),
        ([50,49,47,45,33], True),
    ]
)
def test_run_damper(data, expected):
    assert day02.run_with_damper(data) == expected 

def test_run_b():
    assert day02.run_b(prepped_data) == 4