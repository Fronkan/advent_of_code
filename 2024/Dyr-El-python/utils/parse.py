import re


def parse_lines(inp: str, functions: list[callable], split_groups: str=None):
    """Parses line based input strings

    inp: the input string

    functions: a function to apply to each line in the case of split groups a sequence of functions for every group
    
    split_groups: if not none a delimiter to separate groups of ingut

    Returns: A list of the modified lines or a list of list of modified lines in the case of split groups.
    """
    if split_groups:
        groups = list(inp.split(split_groups))
        group_result = list()
        for func, group in zip(functions, groups):
            group_result.append(list())
            for line in group.split("\n"):
                group_result[-1].append(func(line))
        return group_result
    result = []
    for line in inp.splitlines():
        result.append(functions(line))
    return result
