import sys

lines = open(sys.argv[3], encoding='utf-8').readlines()
head = int(sys.argv[1])
tail = int(sys.argv[2])
if head <= 0:
    head = 1
if head >= len(lines):
    head = len(lines)
if tail > len(lines):
    tail = len(lines)
tlines = lines[head-1:tail]
for idx, line in enumerate(tlines):
    print('\t{}: {}'.format(idx+1, line.rstrip()))
