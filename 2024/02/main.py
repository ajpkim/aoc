from typing import List


def parse_input(file_p: str) -> List[List[int]]:
    with open(file_p, "r") as f:
        lines = f.readlines()
        reports = [[int(num) for num in line.split(" ")] for line in lines]
    return reports


def is_report_safe(report: List[int]) -> bool:
    increasing = True if report[1] > report[0] else False

    for i, n in enumerate(report):
        if i == 0:
            continue

        step = n - report[i - 1]
        # Step magnitude condition
        if abs(step) > 3 or step == 0:
            return False

        # Monotonicity condition
        if (increasing and step < 0) or (not increasing and step > 0):
            return False

    return True


def part_1():
    reports = parse_input("input.txt")
    num_safe_reports = sum(map(is_report_safe, reports))
    print("Part 1:", num_safe_reports)


part_1()
