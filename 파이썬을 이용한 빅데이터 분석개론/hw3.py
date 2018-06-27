# Assignment Number... : 3
# Student Name........ : 허현
# File Name........... : hw3_허현
# Program Description..: 문자열과 리스트, 튜플 활용하는 과제

# 1
steak = 30000
vat = 0.15
print('스테이크의 원래 가격은 {} 원입니다. 하지만 VAT 가 {}%로,\
계산하셔야 할 가격은 {} 원입니다'.format(steak, int(vat*100), int(steak*(1+vat))))


# 2
s = '@^TrEat EvEryonE yOu meet likE you want tO be treated.$%'
s = s[:3]+s[3:].lower()
print(s.alpha())
# 무슨 함수 써야할지 헷갈림...




# 3
numbers = (2,18,26,89,45,39,14)
print(numbers)


# 4
print(len(numbers))


# 5
fruits = ['apple', 'orange', 'strawberry', 'pear', 'kiwi']
print(fruits)


# 6
fruits_sub = fruits[:3]
print(fruits_sub)
