# Assignment Number... : 9
# Student Name........ : 허현
# File Name........... : hw9_허현
# Program Description..: 파일 읽고 쓰기 과제

# 1
## a
car = open("cars.csv", 'r') # cars.csv파일을 읽어서 car 변수에 할당
## b
for line in car: # car에 있는 line 별로 순환을 통해 접근
    print(line) # 각 line을 출력
car.close() # car를 닫음

# 2
## a
car = open('cars.csv', 'r') # cars.csv파일을 읽어서 car 변수에 할당
## b
df = [] # 후에 리스트에 튜플을 추가하기 위해 미리 빈 리스트를 만들어 줌
while True:
    row = car.readline().rstrip() # line별로 car를 읽고 각 line의 끝에 \n이 보기에 좋지 않아 rstrip으로 제거
    if row == '': # readline으로 읽을 것이 더 이상 없을 때 반복문을 탈출
        break
    t = tuple(row.split(',')) # 각 라인을 , 을 기준으로 나누고 튜플화 함
## c
    df.append(t) # 미리 만든 df에 삽입
## d
print(df) # 출력
car.close() # car를 닫음


# 3
## a
way = open('My way.txt', 'r') # My way.txt 파일을 읽어서 way 변수에 할당
## b
for line in way: # way를 line별로 접근
    print(line) # 출력
way.close() # way를 닫음


# 4
## a
way = open('My way.txt', 'r') # # My way.txt 파일을 읽어서 way 변수에 할당
## b
ways = way.readlines() # way를 각 라인별로 리스트의 요소로 넣는 readlines 함수로 읽고 ways에 할당
## c
print(ways[2]) # ways 리스트의 3번째 원소 출력
way.close() # open한 way를 닫음
## d
way = open('My way.txt', 'a') # My way.txt를 수정 모드로 읽어서 way 변수에 할당
way.write("\nI'll state my case, of which I'm certain") # 새로운 문장 작성
way.close() # way를 닫음
way = open('My way.txt', 'r') # 읽기 모드로 읽음
print(way.read()) # 전체를 출력
way.close() # way를 닫음