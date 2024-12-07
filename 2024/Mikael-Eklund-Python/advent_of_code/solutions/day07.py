from advent_of_code.utils.input_parser import parse_integers, parse_str_lines, parse_grid
from itertools import product

def evaluate_left_to_right(expression: str) -> int:
    parts = expression.replace('+', ' + ').replace('*', ' * ').replace('|', ' | ').split()
    result = int(parts[0])
    for i in range(1, len(parts), 2):
        operator = parts[i]
        number = int(parts[i + 1])
        if operator == '+':
            result += number
        elif operator == '*':
            result *= number
        else:
            result = int(str(result) + str(number))
    return result

def part1(input_data: str) -> int:
    """
    Solve Part 1 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 1
    """
    sum = 0
    for row in input_data:
        items = row.split(":")
        number = int(items[0].strip())
        values = items[1].strip().split(" ")
        n = len(values) - 1
        evaluation = ""
        for i in range(2**n):
            combination = ['*' if digit == '1' else '+' 
                          for digit in format(i, f'0{n}b')]
            clen = len(combination)
            for ix, value in enumerate(values):
                if ix < (clen):
                    evaluation += value + combination[ix]
                else:
                    evaluation += value
            result = evaluate_left_to_right(evaluation)
            if result == number:
                sum += number
                break    
            evaluation = ""
    return sum

def part2(input_data: str) -> int:
    """
    Solve Part 2 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 2
    """
    sum = 0
    for row in input_data:
        items = row.split(":")
        number = int(items[0].strip())
        values = items[1].strip().split(" ")
        n = len(values) - 1
        evaluation = ""
        combinations = list(product(['*', '+', '|'], repeat=n))
        for combination in combinations:
            clen = len(combination)
            for ix, value in enumerate(values):
                evaluation += value
                if ix < (clen):
                    evaluation += str(combination[ix])
            result = evaluate_left_to_right(evaluation)
            if result == number:
                sum += number
                break    
            evaluation = ""
    return sum