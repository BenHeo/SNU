# Assignment Number... : 5
# Student Name........ : 허현
# File Name........... : hw5_허현
# Program Description..: 제어문을 활용하는 과제

# 1

a = int(input('Enter a: '))

b = int(input('Enter b: '))

c = int(input('Enter c: '))

 

abc = [a, b, c] # iterable하게 만든다

abc_sum = a+b+c # 세 수를 미리 합한다

abc_max = abc[0]

for i in (1,2):

    if abc[i] > abc_max:

        abc_max = abc[i] # 가장 큰 값을 찾는다

fin_sum = abc_sum - abc_max # 미리 만든 수에서 가장 큰 수를 뺀다

print(fin_sum)

 

# 2

# 문제의 취지가 모호하여 본인이 해석한대로 구성하였습니다

cities = [['Seoul', 605], ['New York', 789], ['Beijing', 16808], ['그 외', 'Unknown']] # 이중리스트로 저장

city = input('Enter the name of the city: ')

for i in range(len(cities)):

    if cities[i][0] == city:

        print('The size of {} is {}'.format(cities[i][0], cities[i][1])) # 입력한 도시가 있을 경우 출력

        break

else:

    print('The size of {} is {}'.format(cities[-1][0], cities[-1][1])) # 입력한 도시가 없었을 경우 출력

 

 

# 3

import math

for i in range(10): # 범위 설정

    print(math.factorial(i)) # factorial 사용

 

# 4

i = 0

while i < 10:

    print(math.factorial(i))

    i += 1



# 번외
class PrimeException(Exception): pass # 에러 처리할 클래스 미리 생성

try:
    prime = int(input('임의의 양의 정수를 입력하세요: '))
    # int로 변환이 안되는 string은 자동으 ValueError 발생
    if prime > 1:
        for i in range(2, prime):
            if prime % i == 0: # 나눠서 나머지가 0이 되는 게 있다면 소수 아닌 것으로 처리
                raise PrimeException()
        else:
            print('이 숫자는 소수입니다.')
    else:
        raise ValueError() # 1보다 작은 경우도 ValueError로 처리
    
except ValueError:
    print('ValueError: 1보다 큰 양의 정수를 입력하세요.')
except PrimeException:
    print('{} x {} = {}'.format(i, prime//i, prime))
    print('이 숫자는 소수가 아닙니다')