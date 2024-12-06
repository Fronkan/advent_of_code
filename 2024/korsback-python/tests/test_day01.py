import pytest
from days import day01

given_test_data = [
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3",
]

sorted_data = [
    [1,3],
    [2,3],
    [3,3],
    [3,4],
    [3,5],
    [4,9]
]

split_data = [
    [1,2,3,3,3,4],
    [3,3,3,4,5,9]
]

def test_prepData():
    assert day01.get_combined_array(given_test_data) == sorted_data

def test_run_a():
    assert day01.run_a(sorted_data) == 11

def test_splitData():
    assert day01.get_left_right(given_test_data) == split_data

def test_run_b():
    assert day01.run_b(split_data) == 31
