from advent_of_code.utils.input_parser import parse_integers, parse_str_lines, parse_grid

def part1(input_data: str) -> int:
    """
    Solve Part 1 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 1
    """
    pos = -1
    current = []
    next_pos = []
    direction = "N"
    walk = {
        "N": [0, -1],
        "S": [0, 1],
        "E": [1, 0],
        "W": [-1, 0]
    }    
    stuck = 0    
    rows = list(input_data)
    for iy, row in enumerate(rows):
        pos = row.lower().find("^", pos + 1)
        if pos != -1:
            current = [pos, iy]
    while True:
        next_pos = current[0] + walk[direction][0], current[1] + walk[direction][1]
        if next_pos[0] < 0 or next_pos[0] >= len(rows[0]):
            sum = 1
            for row in rows:
                sum += row.count("X")
            print(sum)
            return sum
            break
        if next_pos[1] < 0 or next_pos[1] >= len(rows):
            sum = 1
            for row in rows:
                sum += row.count("X")
            print(sum)
            return sum
            break
        if rows[next_pos[1]][next_pos[0]] == "#":
            next_pos = current
            if direction == "N":
                direction = "E"
            elif direction == "E":
                direction = "S"
            elif direction == "S":
                direction = "W"
            elif direction == "W":
                direction = "N"
        else:
            temp_row = list(rows[next_pos[1]])
            temp_row[next_pos[0]] = "X"
            rows[next_pos[1]] = "".join(temp_row)
        current = next_pos

def part2(input_data: str) -> int:
    grid = {}
    grid2 = {}
    free_cords = [(-1,-1)]
    next_cords = []
    no_go = []
    play_cords = []
    direction = "N"
    walk = {
        "N": [0, -1],
        "S": [0, 1],
        "E": [1, 0],
        "W": [-1, 0]
    }  
    summa = 0
    rows = list(input_data)
    for iy, row in enumerate(rows):
        for ix, item in enumerate(row):
            grid2[(ix, iy)] = item
            if item == "^":
                play_cords2 = (ix,iy)
                no_go = (ix -1,iy)
    
    for cords, item in grid2.items():
        if cords != no_go:
            if item == ".":
                free_cords.append(cords)
    print(len(free_cords))
    counts = []  #
    
    def print_grid(g, rows):
        for y in range(len(rows)):
            row = ''
            for x in range(len(rows[0])):
                row += g.get((x,y), ' ')
    
    for i, free_cord in enumerate(free_cords):
        print(i)
        play_cords = play_cords2
        grid = grid2.copy() 
        direction = "N"
        grid[play_cords] = "X"
        
        new_count = 0
        iterations = 0
        
        while True:
            iterations += 1
            
            next_cords = (play_cords[0] + walk[direction][0], play_cords[1] + walk[direction][1])
            if next_cords[0] < 0 or next_cords[0] >= len(rows[0]) or next_cords[1] < 0 or next_cords[1] >= len(rows):
                count = list(grid.values()).count("X")
                counts.append(count + 1)
                break
                
            if next_cords == free_cord or grid[next_cords] == "#":
                if direction == "N": direction = "E"
                elif direction == "E": direction = "S"
                elif direction == "S": direction = "W"
                elif direction == "W": direction = "N"
            else:
                grid[next_cords] = "X"
                play_cords = next_cords
                count = list(grid.values()).count("X")
                if new_count == count:
                    iterations += 1
                else: 
                    iterations = 0
                new_count = count
                if iterations > 100:
                    print("Loop detected")
                    summa += 1
                    iterations = 0
                    break
                    
    return summa 