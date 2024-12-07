from advent_of_code.utils.input_parser import parse_integers, parse_str_lines, parse_grid

def part1(input_data: str) -> int:
    """
    Solve Part 1 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 1
    """
    rules = []
    pages = []
    prints = ""
    sum = 0
    for row in input_data:
        items = row.split("|")
        if len(items) == 2:
            rules.append(row)
        items = row.split(",")
        if len(items) >= 2:
            pages.append(row)
    for page in pages:
        passes = True
        prints = page.split(",")
        for ix, item in enumerate(prints):
            if ix < (len(prints) - 1):
                test = item + "|" + prints[ix + 1]
                if test not in rules:
                    passes = False;
                    break
        if passes:
            testen = page.split(",")
            middle_item = testen[len(testen) // 2] 
            sum += int(middle_item)
    return sum

def part2(input_data: str) -> int:
    """
    Solve Part 2 of the day's challenge.
    
    Args:
        input_data (str): Puzzle input
    
    Returns:
        int: Solution for Part 2
    """
    rules = []
    pages = []
    prints = ""
    sum = 0
    for row in input_data:
        items = row.split("|")
        if len(items) == 2:
            rules.append(row)
        items = row.split(",")
        if len(items) >= 2:
            pages.append(row)
    for page in pages:
        passes = True
        prints = page.split(",")
        for ix, item in enumerate(prints):
            if ix < (len(prints) - 1):
                test = item + "|" + prints[ix + 1]
                if test not in rules:
                    passes = False
                    break
        if not passes:
            items = list(prints)
            items.sort(key=int, reverse=True)
            testen = {}
            for item in items:    
                search_term = item + "|"
                matches = [i for i, rule in enumerate(rules) if search_term in rule]
                if matches:
                    temp_row = []
                    count = 0
                    for pos in matches:
                        if rules[pos].split("|")[1] in prints:
                            temp_row.append(rules[pos].split("|")[1])
                            count += 1
                    testen[count] = item
            diffen = (len(items) - len(testen))
            sorted_list = [int(testen[key]) for key in sorted(testen.keys(), reverse=True)] 
            if diffen == 1:
                x = (len(testen)//2)
                sum += sorted_list[x]
            else:
                x = (len(testen)//2)
                sum += sorted_list[x]
    return sum