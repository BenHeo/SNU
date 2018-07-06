# 출석 키우기 게임
import numpy as np

def absent4checker(absent, late): # 결석과 지각3번으로 인한 결석처리를 합쳐서 4회가 넘는지 알려주는 역할을 한다.
    changed_late = late // 3 # 지각은 3번이 쌓여야만 absent 처리가 되므로 3으로 나눈 정수부분만 구한다
    if absent + changed_late >= 4:
        return True
    else:
        return False


late = 0 # 지각한 횟수
absent = 0 # 결석한 횟수
absent_check = 0 # absent4checker 함수를 반복해서 받아줄 변수명이다
print("""게임 규칙:
      \t(1) 결석이 4번에 도달하는 즉시 점수는 F다. 
      \t(2) 지각이나 조퇴가 3번이면 결석 1번이다.
      \t(3) 지각한 날에 조퇴하면 결석이다.
      \t(4) 16번의 수업이 다 끝나는 동안 F가 아니면 P이다.""")
print()
name = input('플레이어의 이름(Full Name)을 입력하세요.: ')

while name == '박진수':
    print('교수님은 출석체크를 당하지 않습니다.')  # 이스터에그
    print()
    name = input('플레이어의 이름(Full Name)을 입력하세요.: ')

turn = 0
while turn <= 15 and not absent_check: # 총 16주 수업을 반복하고 그 중 결석 4회 이상으로 처리되면 게임이 종료된다
    turn += 1
    act = int(input("""
    {}번째 수업입니다. 수업 참여 여부에 대해 알려주세요.
    \t1. 정상 참여
    \t2. 지각
    \t3. 결석
    \t 입력: """.format(turn)))
    if act == 3: # 결석하면 결석 수를 늘리고 absent4checker를 수행해서 변동여부를 알아보고 다시 반복문을 수행하게 한다
        absent += 1
        print('현재 {}번 결석했습니다.'.format(absent))
        absent_check = absent4checker(absent, late)
        continue

    if act == 2: # 지각하면 지각을 추가한다
        late += 1

    get_out = int(input("""당신은 수업을 계속해서 들을 겁니까?    # 정상 참여 했거나 지각한 경우 끝까지 들을지 다시 묻는다
    \t1. 계속 듣는다.
    \t2. 조퇴한다.
    \t 입력: """))
    if get_out == 2:
        if act == 1:
            print('조퇴했습니다.')
            late += 1 # 정상 참여한 사람은 조퇴시 지각1회와 같이 처리한다
        else:
            print('지각한 주제에 조퇴했으므로 결석 처리되었습니다.')
            absent += 1 # 지각한 날 조퇴한 사람은 결석 처리된다
    absent_check = absent4checker(absent, late) # 다음 반복문을 돌리기 전에 결석정도를 업데이트 한다

print('\n{}씨'.format(name))

if absent_check:
    print('출결 결과 당신은 F입니다.') # 결석이 많아서 while문이 끝났을 때
else:
    print('출결 점수는 Pass했습니다. 시험과 과제에 신경쓰세요.') # 결석이 많지 않고 while문이 끝났을 때