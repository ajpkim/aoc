import re

pat = r"(\d+)\s+(\d+)"

with open("input.txt", "r") as f:
    left = []
    right = []
    for line in f.readlines():
        match = re.match(pat, line)
        n1, n2 = match.groups()

        left.append(int(n1))
        right.append(int(n2))

left.sort()
right.sort()

res = 0
for a, b in zip(left, right):
    res += abs(a - b)

print(res)
