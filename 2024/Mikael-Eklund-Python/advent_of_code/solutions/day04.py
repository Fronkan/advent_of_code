from advent_of_code.utils.input_parser import parse_integers, parse_str_lines, parse_grid

def part1(input_data: str) -> int:
    """
    Solve Part 1 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 1
    """
    sum = 0
    pos = -1
    rows = input_data
    temp_row = ""
    vert_input = []
    lrows = len(rows)
    for ix, row in enumerate(rows[0]):
        for row in rows:
            temp_row = temp_row + row[ix]
        vert_input.append(temp_row)
        temp_row = ""
    rows = input_data
    for row in rows:
        while True:
            pos  = row.lower().find("xmas", pos + 1)
            if pos == -1:
                break
            sum = sum + 1
        pos = -1
        while True:
            pos  = row.lower().find("samx", pos + 1)
            if pos == -1:
                break
            sum = sum + 1
    
    for row in vert_input:
        while True:
            pos  = row.lower().find("xmas", pos + 1)
            if pos == -1:
                break
            sum = sum + 1
        pos = -1
        while True:
            pos  = row.lower().find("samx", pos + 1)
            if pos == -1:
                break
            sum = sum + 1

    for x in range(lrows - 3):
        for y in range(lrows - 3):
            row1 = rows[x][y]
            row2 = rows[x + 1][y + 1]
            row3 = rows[x + 2][y + 2]
            row4 = rows[x + 3][y + 3]

            if row1 == "X" and row2 == "M" and row3 == "A" and row4 == "S":
                sum = sum + 1
            if row1 == "S" and row2 == "A" and row3 == "M" and row4 == "X":
                sum = sum + 1

            row1 = rows[x][y +3 ]
            row2 = rows[x + 1][y + 2]
            row3 = rows[x + 2][y + 1]
            row4 = rows[x + 3][y]

            
            if row1 == "X" and row2 == "M" and row3 == "A" and row4 == "S":
                sum = sum + 1
            if row1 == "S" and row2 == "A" and row3 == "M" and row4 == "X":
                sum = sum + 1
    


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
    row1 = ""
    row2 = ""
    row3 = ""
    trow1  = "M M"
    trow2  = " A "
    trow3  = "S S"

    trow11 = "M S"
    trow2  = " A "
    trow31 = "M S"

    trow12 = "S M"
    trow2  = " A "
    trow32 = "S M"

    trow13 = "S S"
    trow2  = " A "
    trow33 = "M M"
    rows = input_data
    lrows = len(rows)
    for x in range(lrows - 2):
        for y in range(lrows - 2):
            row1 = rows[x][y] + " " + rows[x][y + 2]
            row2 = " " + rows[x + 1][y + 1] + " "
            row3 = rows[x + 2][y] + " " + rows[x + 2][y + 2]
            if row1 == trow1 and row2 == trow2 and row3 == trow3:
                sum = sum + 1
            if row1 == trow11 and row2 == trow2 and row3 == trow31:
                sum = sum + 1
            if row1 == trow12 and row2 == trow2 and row3 == trow32:
                sum = sum + 1
            if row1 == trow13 and row2 == trow2 and row3 == trow33:
                sum = sum + 1
    return sum