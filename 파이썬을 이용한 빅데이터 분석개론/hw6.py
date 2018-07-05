# Assignment Number... : 6
# Student Name........ : 허현
# File Name........... : hw6_허현
# Program Description..: 함수를 활용하는 과제

# 1
## a
def area_triangle(h, w): # 각 인자를 h와 w로 설정
## b
    return 0.5*h*w # 삼각형을 구하는 공식 적용
## c
my_tri = area_triangle(10, 15) # 함수 활용
print(my_tri)


# 2
## a
def distance(a, b):
    square_sum = 0 # 제곱해서 더하는 값을 이 변수에 더할 것
## b
    for i in range(2):
        square_temp = (a[i] - b[i])**2 # 수의 차를 구한 값에 제곱을 함
        square_sum += square_temp # 제곱한 값을 square sum에 더함. 생략하고 한 줄로 줄여 쓸 수 있지만 이해를 위해 두 줄로 씀
## c
    return square_sum ** 0.5 # 거리 제곱을 루트 씌워서 반환
## d
a = (1,2)
b = (5,7)
ab_dist = distance(a, b)
print(ab_dist)


# 3
## a
def count(n):
## b
    if n > 0: # n이 0보다 큰 경우 다음을 수행
        print(n) # n을 출력
        count(n-1) # 재귀적 구문으로 count에서 n-1을 수행
## c
    elif n == 0: # 문제에서는 else를 요청했지만 명확한 개념 전달을 위해 elif 활용함
        print('zero!!')
## d
count(5)


# 4
area_triangle_ld = lambda h, w : 0.5*h*w # 변수에 lambda 함수 활용한 값 할당
my_tri_ld = area_triangle_ld(10,15) # 10, 15 대입
print(my_tri_ld)
