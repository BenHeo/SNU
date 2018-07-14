# Assignment Number... : 10
# Student Name........ : 허현
# File Name........... : hw10_허현
# Program Description..: 파일 읽고 자료 처리하는 과제

lines = open('subway.txt', encoding='utf-8-sig').readlines() # utf-8로 했을 때 \ufeff가 생기는 문제가 발생하여 검색 결과 해결책인 utf-8-sig 추가
names = [name.strip() for name in lines[0].split(',')] # 첫 줄의 내용을 후에 딕셔너리의 이름으로 사용하기 위해 미리 저장
                                                       # 별도의 제거가 필요한 character들을 제거하기 위해 strip 사용
subway_data = [] # 딕셔너리들을 저장할 리스트 생성
for i in range(1, len(lines)): # 첫 줄을 제외한 나머지 줄에 대한 반복 수행
    splt_line = [name.strip() for name in lines[i].split(',')] # 첫 줄에 대해 했던 것처럼 별도로 분류
    data = dict() # 빈 딕셔너리 생성
    i = 0 # for문을 두번 쓰는 낭비를 줄이기 위해 인덱스를 미리 생성
    for name in names: # 미리 만들어둔 names 속 컬럼명 순회
        data[name] = splt_line[i] # 딕셔너리에 키는 컬럼명, 값은 i번째 splt_line이 되게 저장
        i+=1 # splt_line의 인덱스를 다음 인덱스로 넘김
    subway_data.append(data) # 다 만들어진 딕셔너리를 subway_data에 저장 


# 승차인원이 가장 많은 날과 승차인원 수를 출력
max_idx = 0 # 가장 많은 날의 인덱스를 저장하기 위함
max_in = 0 # 가장 많은 날의 승차인원 수를 저장하기 위함
times = ['7', '8', '9', '10', '11'] # 모든 시간대를 미리 저장
for i in range(0,len(subway_data),2): # 승차인원이 궁금한 것이므로 짝수번째만 순회
    time_num = [int(subway_data[i][time]) for time in times] # 각 시간대별 사람 수를 저장
    row_sum = sum(time_num) # 위의 수의 총합을 구함
    if row_sum > max_in: # 기존 max보다 클 경우 인덱스를 저장하고 그 수를 저장함
        max_idx = i
        max_in = row_sum
print('\n승차인원이 가장 많은 날의 정보')
print(subway_data[max_idx])
print('가장 승차인원이 많은 날의 승차인원 수는 {}명입니다.'.format(max_in))

print('\n============================================================\n') # 명확한 줄 구분 위해

# 하차인원이 가장 많은 날과 하차인원 수를 출력
max_idx = 0 # 가장 많은 날의 인덱스를 저장하기 위함
max_in = 0 # 가장 많은 날의 하차인원 수를 저장하기 위함
times = ['7', '8', '9', '10', '11'] # 모든 시간대를 미리 저장
for i in range(1,len(subway_data),2): # 하차인원이 궁금한 것이므로 홀수번째만 순회
    time_num = [int(subway_data[i][time]) for time in times] # 각 시간대별 사람 수를 저장
    row_sum = sum(time_num) # 위의 수의 총합을 구함
    if row_sum > max_in: # 기존 max보다 클 경우 인덱스를 저장하고 그 수를 저장함
        max_idx = i
        max_in = row_sum
print('\n하차인원이 가장 많은 날의 정보')
print(subway_data[max_idx])
print('가장 하차인원이 많은 날의 하차인원 수는 {}명입니다.'.format(max_in))

print('\n============================================================\n') # 명확한 줄 구분 위해

# 주말을 모두 출력하고 토요일 평균 승, 하차인원과 일요일 평균 승, 하차인원 출력
saturday_in = 0 # 토요일 승차인원
saturday_out = 0 # 토요일 하차인원
sunday_in = 0 # 일요일 승차인원
sunday_out = 0 # 일요일 하차인원
saturday_cnt = 0 # 토요일 총 수
sunday_cnt = 0 # 일요일 총 수
saturday_flag = 0 # 토요일 안에도 승차와 하차가 있기 때문에 한 번만 세기 위해 flag를 활용
sunday_flag = 0
total_len = len(subway_data) # while 반복문을 사용할 것인데 조건에 계속 리스트의 길이를 구하게 하는 컴퓨팅 낭비를 막기 위해 미리 저장
times = ['7', '8', '9', '10', '11'] # 모든 시간대를 미리 저장

# 데이터를 보면 토요일 승차부터 시작되는 것을 알 수 있음
i = 0 # i는 주에 대한 정보를 담음 ex) i=0 means 0주차(문맥상 1주차)
j = 0 # j는 주 내에서 요일에 대한 정보를 담음
print('\n모든 주말 정보')
while 14*i + j < total_len: # 수행하는 주와 요일 수의 합이 총 수보다 적거나 같은 동안(코딩 상으로는 인덱스가 0부터 시작하기 때문에 <로 표기)
    print(subway_data[14*i + j]) # 주말 정보 출력
    time_num = [int(subway_data[14*i + j][time]) for time in times] # 각 시간대별 사람 수를 저장
    row_sum = sum(time_num) # 위의 수의 총합을 구함

    if j == 0: # 토요일 승차 정보가 들어간 인덱스
        saturday_in += row_sum
        saturday_cnt += 1
        saturday_flag = 1 # 수를 셌음
    elif j == 1: # 토요일 하차 정보가 들어간 인덱스
        saturday_out += row_sum
        if saturday_flag: # 이미 토요일 수를 셌다면 더 세지 않겠다
            saturday_cnt += 0
            saturday_flag = 0 # 다음 주를 위해 다시 세지 않았음으로 바꿔줌
        else:
            saturday_cnt += 1
            saturday_flag = 0 # 다음 주를 위해 다시 세지 않았음으로 바꿔줌
    elif j == 2: # 일요일 승차 정보가 들어간 인덱스
        sunday_in += row_sum
        sunday_cnt += 1
        sunday_flag = 1 # 수를 셌음
    elif j == 3: # 일요일 하차 정보가 들어간 인덱스
        sunday_out += row_sum
        if sunday_flag: # 이미 일요일 수를 셌다면 더 세지 않겠다
            sunday_cnt += 0
            sunday_flag = 0 # 다음 주를 위해 다시 세지 않았음으로 바꿔줌
        else:
            sunday_cnt += 1
            sunday_flag = 0 # 다음 주를 위해 다시 세지 않았음으로 바꿔줌

    j += 1 # 매번 j를 늘려야 반복 가능
    if j == 4: # 필요한 j의 정보는 0~4이기 때문에 5가 될 때 i는 1 추가하고 j는 0으로 만듦
        i += 1
        j = 0
print('\n요약 주말 정보')
print('토요일 평균 승차인원은 {}명, 평균 하차인원은 {}명입니다.'.format(saturday_in/saturday_cnt, saturday_out/saturday_cnt))
print('일요일 평균 승차인원은 {}명, 평균 하차인원은 {}명입니다.'.format(sunday_in/sunday_cnt, sunday_out/sunday_cnt))

        
    
