from typing import List, Optional


def parse_input(file_p: str) -> List[List[int]]:
    with open(file_p, "r") as f:
        lines = f.readlines()
        reports = [[int(num) for num in line.split(" ")] for line in lines]
    return reports


def is_report_safe(report: List[int]) -> bool:
    if len(report) < 2:
        return True

    increasing = True if report[1] > report[0] else False

    for i in range(1, len(report)):
        step = report[i] - report[i - 1]
        # Step magnitude condition
        if abs(step) > 3 or step == 0:
            return False
        # Monotonicity condition
        if (increasing and step < 0) or (not increasing and step > 0):
            return False

    return True


def is_report_safe_with_dampener(report: List[int], dampener: bool = True) -> bool:
    if is_report_safe(report):
        return True
    elif dampener == False:
        return False
    else:
        for i in range(len(report)):
            modified_report = report[:i] + report[i + 1 :]
            if is_report_safe_with_dampener(modified_report, dampener=False):
                return True

    return False


def part_1():
    reports = parse_input("input.txt")
    num_safe_reports = sum(map(is_report_safe, reports))
    print("Part 1:", num_safe_reports)


def part_2():
    reports = parse_input("input.txt")
    num_safe_reports = sum(map(is_report_safe_with_dampener, reports))
    print("Part 1:", num_safe_reports)


part_1()
part_2()
