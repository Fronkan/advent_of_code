import helpers

def start(filePath, part):
    data = helpers.read_file_lines(filePath)
    if part == "a":
        return run_a(get_combined_array(data))
    elif part == "b":
        return run_b(get_left_right(data))

def get_combined_array(data):

    left, right = get_left_right(data)

    result = []
    for i in range(len(left)):
        result.append([left[i], right[i]])
    
    return result

def get_left_right(data):
    left = []
    right = []
    for line in data:
        list = [int(x) for x in line.split()]
        left.append(list[0])
        right.append(list[1])
    
    left.sort()
    right.sort()

    return [left, right]

def run_a(data):
    sum = 0

    for pair in data:
        sum += abs(pair[0] - pair[1])
    
    return sum

def run_b(data):
    sum = 0
    
    left = data[0]
    right = data[1]

    for i in range(len(left)):
        occurences = right.count(left[i])
        sum += occurences*left[i]

    return sum
