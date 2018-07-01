# Assignment Number... : 3
# Student Name........ : 허현
# File Name........... : hw3_허현
# Program Description..: 제어문과 딕셔너리를 활용하는 과제


# 1
restaurant_list = [{'상호': 'A', '메뉴': '피자', '가격(원)': 20000},
                    {'상호': 'B', '메뉴': '치킨', '가격(원)': 18000},
                     {'상호': 'C', '메뉴': '짜장면', '가격(원)': 5000},
                      {'상호': 'D', '메뉴': '초밥', '가격(원)': 15000},
                       {'상호': 'E', '메뉴': '치킨', '가격(원)': 23000},
                        {'상호': 'F', '메뉴': '족발', '가격(원)': 30000}]

# 각 키에 해당하는 value들은 자동으로 가격은 int형, 나머지는 str형이기 때문에 별도 입력 X

want_to_eat = input('먹고 싶은 음식을 입력하세요 : ') # 메뉴 입력
flag = 1 # 메뉴가 있는지 체크하기 위한 변수
for i in range(len(restaurant_list)):
    if restaurant_list[i]['메뉴'] == want_to_eat:
        flag = 0 # 있을 경우이므로 flag 를 0으로 설정
        print('식당 {}, 가격 {} 원'.format(restaurant_list[i]['상호'], restaurant_list[i]['가격(원)']))
        # 딕셔너리 형식에 맞게 출력
if flag:
    # flag가 0으로 나왔으므로 다음과 같이 출력
    print('결과가 없습니다.') 
