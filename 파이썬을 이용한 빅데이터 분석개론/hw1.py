# Assignment Number... : 1
# Student Name........ : #반드시 이름은 한글로 작성할 것
# File Name........... : hw1_허현
# Program Description..: 자료형 활용하는 과제

# 1
season = input("What is your favorite season?") # season을 출력하기 위해 할당했다
print(season)


# 2
date1 = input("Which date were you born?") # date변수는 위험할 수 있어서 date1으로 지정했다
print(date1)


# 3
print(type(date1)) # type 함수로 타입을 알아봤다


# 4
date1 = float(date1) # 타입을 바꾸고 다시 저장했다
print(type(date1))


# 5
print("I love", season, "and I was born at", int(date1)) # string과 기존 저장된 것을 동시에 출력하기 위해 ,를 이용했다
                                                        # date1이 현재 float이라 int로 바꿨다.
