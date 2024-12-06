def read_file_lines(path):
    with open(path) as f:
        return [item.strip() for item in f.read().splitlines()]