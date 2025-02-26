import re

with open("input.txt", "r") as f:
    text = f.read()

# Find all the mul instructions and capture the 2 digits in each
pat = r"mul\((\d{1,3}),(\d{1,3})\)"
res = sum([int(x) * int(y) for (x, y) in re.findall(pat, text)])

print("Part 1:", res)


### Non-regex ###

with open("input.txt", "r") as f:
    text = f.read()


"""Sliding window approach

- We start a window when we get to 'm' and then we continue on expanding
our window so long as the subsequent chars are on track for a valid mul instruction.
- We can try to parse the digit spaces to numbers and if it fails then we move on
- Once we find "mul(", we try making a number that's 1-3 digits.
- If we get the 1st num, then we check for a ',' and try to make a 1-3 digit number
  again and then close the instruction with ')'.
"""
res = 0
a = b = None
window = ""
comma = False

for i, ch in enumerate(text):
    if not window:
        if ch == "m":
            window += ch

    elif window == "m" and ch == "u":
        window += ch

    elif window == "mu" and ch == "l":
        window += ch

    elif window == "mul" and ch == "(":
        window += ch

    elif window == "mul(" and not a and ch.isdigit():
        a = ch

    elif a and not comma and len(a) < 3 and ch.isdigit():
        a += ch

    elif a and not b and ch == ",":
        comma = True

    elif a and comma and not b and ch.isdigit():
        b = ch

    elif b and len(b) < 3 and ch.isdigit():
        b += ch

    elif b and ch == ")":
        res += int(a) * int(b)
        window = ""
        a = b = None
        comma = False

    else:
        window = ""
        a = b = None
        comma = False

print("Part 1:", res)

"""
Recursively?

We need to know the

"""
