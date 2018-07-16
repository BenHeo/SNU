import sys
f = sys.argv[2]
lines = open(f, encoding='utf-8').readlines()
finder = sys.argv[1]
for i, line in enumerate(lines):
    if finder in line:
        print("{:4}: {}".format(i, lines[i].rstrip()))