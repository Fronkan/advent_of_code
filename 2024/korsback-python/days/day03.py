import helpers
import re

def start(filePath, part):
    data = helpers.read_file_lines(filePath)
    if part == "a":
        return run_a(data[0])
    elif part == "b":
        return run_b(data[0])

def parse_mul_instructions(data):
    result = []

    iter = re.finditer(r"(mul\((\d+),(\d+)\))", data)

    for match in iter:
        result.append([int(match.group(2)), int(match.group(3))])

    return result

def parse_mul_instructions_with_do_dont(data):
    result = []

    iter = re.finditer(r"(mul\((\d+),(\d+)\))|(do\(\))|(don't\(\))", data)

    do = True
    for match in iter:
        if match.group(1) and do:
            result.append([int(match.group(2)), int(match.group(3))])
        elif match.group(4):
            do = True
        elif match.group(5):
            do = False

    return result

def sum_values(pairs):
    sum = 0
    for x in pairs:
        sum += (x[0] * x[1])

    return sum

def run_a(data):
    return sum_values(parse_mul_instructions(data))

def run_b(data):
    return sum_values(parse_mul_instructions_with_do_dont(data))