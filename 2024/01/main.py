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

print("Part 1:", res)

res = 0
num_counts = {}

for n in right:
    if n not in num_counts:
        num_counts[n] = 0
    num_counts[n] += 1

for n in left:
    res += n * num_counts.get(n, 0)

print("Part 2:", res)
