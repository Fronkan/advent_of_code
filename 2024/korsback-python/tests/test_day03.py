import pytest
from days import day03

test_data = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
test_data2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

def test_pars_mul_instructions():
    expected = [[2,4], [5,5], [11,8], [8,5]]

    assert day03.parse_mul_instructions(test_data) == expected

def test_sum_values():
    data = [[2,4], [5,5], [11,8], [8,5]]
    expected = 161
    assert day03.sum_values(data) == expected

def test_run_a():
    assert day03.run_a(test_data) == 161

def test_parse_mul_instructions_with_do_dont():
    assert day03.parse_mul_instructions_with_do_dont(test_data2) == [[2,4],[8,5]]