import helpers

def start(filePath, part):
    data = helpers.read_file_lines(filePath)
    if part == "a":
        return run_a(get_prepped_lines(data))
    elif part == "b":
        return run_b(get_prepped_lines(data))

def get_prepped_lines(data):
    return [list(map(int, line.split())) for line in data]

def get_is_safe(line):

    length = len(line)
    is_positive_increment = get_direction(line[0], line[length-1])

    for i in range(length-1):
        diff = abs(line[i]-line[i+1])
        if ((is_positive_increment and line[i] > line[i+1]) or (not is_positive_increment and line[i] < line[i+1])):
            return False
        elif (diff > 3 or diff == 0):
            return False

    return True

def get_direction(a, b):
    return a < b

def run_with_damper(line):

    if(get_is_safe(line)):
        return True

    for i in range(len(line)):
        tmp = line[:]
        tmp.pop(i)
        if get_is_safe(tmp):
            return True
    
    return False

def run_a(data):
    found_safe = 0
    
    for line in data:
        if get_is_safe(line): 
            found_safe += 1

    return found_safe

def run_b(data):
    found_safe = 0

    for line in data:
        if(run_with_damper(line)):
            found_safe += 1

    return found_safe
    