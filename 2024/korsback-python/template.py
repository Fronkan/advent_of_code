import helpers

def start(filePath, part):
    data = helpers.read_file_lines(filePath)
    if part == "a":
        return run_a()
    elif part == "b":
        return run_b()

def run_a(data):
    return 0

def run_b(data):
    return 0