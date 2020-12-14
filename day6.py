from itertools import groupby
from functools import reduce
from collections import Set

def part2(groups):
    sets = [[set(l) for l in g] for g in groups]
    intersections = [reduce(lambda a,b: a.intersection(b), g) for g in sets]
    return sum(len(g) for g in intersections)

def splitOn(l, sep):
    return [list(group) for k, group in groupby(l, lambda x: x == sep) if not k]

def parse(input_):
    return splitOn(input_.splitlines(), "")

def doDay6():
    with open("inputs/day6.txt") as f:
        input_ = f.read()

    print(part2(parse(input_)))

doDay6()
