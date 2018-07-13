import sys

lines = open(sys.argv[2], encoding='utf-8').readlines()
tlines = lines[-int(sys.argv[1]):]
for idx, line in enumerate(tlines):
    print('\t{}: {}'.format(idx+1, line.rstrip()))
