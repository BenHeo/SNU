import sys

lines = open(sys.argv[2], encoding='utf-8').readlines()
for idx, line in enumerate(lines):
    if idx == int(sys.argv[1]):
        break
    print('\t{}: {}'.format(idx+1, line.rstrip()))
