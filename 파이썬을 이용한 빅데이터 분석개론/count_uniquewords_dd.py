from collections import defaultdict

f = input('파일 이름을 입력하세요...: ')
lines = open(f, encoding='utf-8').readlines()
words = defaultdict(lambda: 0) # 사실 이거는 그냥 defaultdict(int)로 하면 0을 기본값으로 함
# defaultdictionary로 키 값이 없을 때 default 값을 lambda 이용해서 0을 주었음
for line in lines:
    words_in_line = line.split()
    for i in range(len(words_in_line)):
        words_in_line[i] = words_in_line[i].rstrip()
        words_in_line[i] = words_in_line[i].replace('-', '')
        words_in_line[i] = words_in_line[i].replace('=', '')
        if words_in_line[i] == '':
            continue
        words[words_in_line[i]] += 1
        # default가 0이기 때문에 처음 들어온 값은 바로 0이었다가 1이 더해지고, 원래 있던 값은 1이 더해져서 count됨

        # 위의 if - else 문을 get함수를 사용하면 더 쉽다 words[words_in_line[i]] = words.get(words_in_line[i], 0) + 1

for k, v in sorted(words.items()):
    print('<{}> : {}번 출현'.format(k, v))