f = input('파일 이름을 입력하세요...: ')
lines = open(f, encoding='utf-8').readlines()
words = dict()
for line in lines:
    words_in_line = line.split()
    for i in range(len(words_in_line)):
        words_in_line[i] = words_in_line[i].rstrip()
        words_in_line[i] = words_in_line[i].replace('-', '')
        words_in_line[i] = words_in_line[i].replace('=', '')
        if words_in_line[i] == '':
            continue
        if words_in_line[i] in words.keys():
            words[words_in_line[i]] += 1
        else:
            words[words_in_line[i]] = 1
        # 위의 if - else 문을 get함수를 사용하면 더 쉽다 words[words_in_line[i]] = words.get(words_in_line[i], 0) + 1

for k, v in sorted(words.items()):
    print('<{}> : {}번 출현'.format(k, v))