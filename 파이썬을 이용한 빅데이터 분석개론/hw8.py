# Assignment Number... : 8
# Student Name........ : 허현
# File Name........... : hw8_허현
# Program Description..: 패키지와 모듈을 활용하는 과제

# 1
## a
import datetime
## b
now = datetime.datetime.now() # datetime을 이용한 현재 시간 구하기
## c
print(now.strftime('%Y-%m-%d %H:%M:%S')) # strftime 기능을 활용해서 포맷에 맞게 출력시키기

# 2
## a
import calendar
## b
print(calendar.isleap(2050)) # 윤년인지 알려주는 isleap 함수에 2050을 넣어 확인한다
## c
print(calendar.weekday(2050, 7, 7)) # 일주일 중 몇요일인지를 수로 반환하는 weekday 함수를 사용한다

# 3
import collections
## a
def vowel(sentence):
## b
    count = collections.Counter(sentence) # sentence로 받은 문자열의 캐릭터를 Counter를 통해 센다
    vowels = ['a', 'e', 'i', 'o', 'u'] # 모음 리스트를 미리 만들어 둔다(소문자만)
    for i in vowels:
        print("The numer of", i, ":", count[i]) # 모음 리스트를 순환시키면서 해당 모음이 count에 얼마나 있었는지 센다
    ordered_count = count.most_common() # 후에 순서대로 정렬한 것을 활용하기 위해 미리 순서대로 정렬
    most_vowel = 'x' # 가장 많이 사용된 모음을 저장하기 위한 dummy variable 생성
    for i in ordered_count:
        if i[0] in vowels:
            most_vowel = i[0] # 순서대로 정렬한 리스트에서 가장 먼저 나오는 모음이 가장 많이 사용된 모음이기 때문에 저장
            break # 더 이상 순환할 필요 없으므로 break
## c
    sentence = sentence.replace(most_vowel, most_vowel.upper()) # 기존 가장 많이 사용된 모음을 대문자로 바꿔서 replace
    print(sentence)
## d
vowel('The regret after not doing something is bigger than that of doing something') # 문장 넣어서 사용