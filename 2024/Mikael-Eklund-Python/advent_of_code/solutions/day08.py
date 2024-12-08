from advent_of_code.utils.input_parser import parse_integers, parse_str_lines, parse_grid

def manhattan_distance(p1, p2):
    return abs(p2[0] - p1[0]) + abs(p2[1] - p1[1])

def find_third_point(p1, p2):
    dx = p2[0] - p1[0]
    dy = p2[1] - p1[1]
    p3_same = (p2[0] + dx, p2[1] + dy)
    p3_opposite = (p1[0] - dx, p1[1] - dy)
    return p3_same, p3_opposite

def find_all_points(p1, p2, grid):
    points_same = []
    points_opposite = []
    current_p1, current_p2 = p1, p2
    while True:
        dx = current_p2[0] - current_p1[0]
        dy = current_p2[1] - current_p1[1]
        next_point = (current_p2[0] + dx, current_p2[1] + dy)
        if next_point in grid:
            points_same.append(next_point)
            current_p1, current_p2 = current_p2, next_point
        else:
            break
    current_p1, current_p2 = p1, p2
    while True:
        dx = current_p2[0] - current_p1[0]
        dy = current_p2[1] - current_p1[1]
        next_point = (current_p1[0] - dx, current_p1[1] - dy)
        if next_point in grid:
            points_opposite.append(next_point)
            current_p2, current_p1 = current_p1, next_point
        else:
            break
            
    return points_same, points_opposite

def part1(input_data: str) -> int:
    """
    Solve Part 1 of the day's challenge.
    
    Args:if p3_same in grid and grid[pif p3_same in grid and grid[p3_same] == ".":
                        grid[p3_same] = "#"3_same] == ".":
                        grid[p3_same] = "#"
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 1
    """
    grid = {}
    used_spots = {}
    rows = list(input_data)
    for iy, row in enumerate(rows):
        for ix, item in enumerate(row):
            grid[(ix, iy)] = item
    
    for cords, item in grid.items():
        if item != "." and item != "#":
            for cords2, item2 in grid.items():
                if item2 == item and cords != cords2:
                    p3_same, p3_opposite = find_third_point(cords, cords2)
                    if p3_same in grid and grid[p3_same] == ".":
                        grid[p3_same] = "#"
                    elif p3_same in grid and grid[p3_same] != item and grid[p3_same] != "#":
                        if p3_same not in used_spots:
                            used_spots[p3_same] = item
                    if p3_opposite in grid and grid[p3_opposite] == ".":
                        grid[p3_opposite] = "#"
                    elif p3_opposite in grid and grid[p3_opposite] != item and grid[p3_opposite] != "#":
                        if p3_opposite not in used_spots:
                            used_spots[p3_opposite] = item
    
    count = list(grid.values()).count("#")
    sum = count + len(used_spots)
    
    return sum

def part2(input_data: str) -> int:
    """
    Solve Part 2 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 2
    """
    grid = {}
    used_spots = {}
    rows = list(input_data)
    for iy, row in enumerate(rows):
        for ix, item in enumerate(row):
            grid[(ix, iy)] = item
    dot_count = sum(1 for value in grid.values() if value != ".")
    for cords, item in grid.items():
        if item != "." and item != "#":
            for cords2, item2 in grid.items():
                if item2 == item and cords != cords2:
                    p3_same_list, p3_opposite_list = find_all_points(cords, cords2, grid)
                    for p3_same in p3_same_list:    
                        if p3_same in grid and grid[p3_same] == ".":
                            grid[p3_same] = "#"
                        elif p3_same in grid and grid[p3_same] != item and grid[p3_same] != "#":
                            if p3_same not in used_spots:
                                used_spots[p3_same] = item
                    for p3_opposite in p3_opposite_list:
                        if p3_opposite in grid and grid[p3_opposite] == ".":
                            grid[p3_opposite] = "#"
                        elif p3_opposite in grid and grid[p3_opposite] != item and grid[p3_opposite] != "#":
                            if p3_opposite not in used_spots:
                                used_spots[p3_opposite] = item
    count = list(grid.values()).count("#")
    summa = count + dot_count
    
    return summa