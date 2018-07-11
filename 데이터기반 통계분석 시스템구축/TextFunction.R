# paste(0) 함수
paste("감자로","만든","감자칩", sep='-')
paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
paste0(1:12, collapse = "-") # collapse는 다음 것으로 넘어갈 때 뭘 사이에 두고 넘어갈지
paste(1:4,5:8, sep = ';') # sep은 붙일 때 어떻게 붙일지
paste(1:4,5:8, sep = ';', collapse = '-')

# grep 함수 # 문자열에 패턴(단어)가 포함되어 있는지를 확인하는 함수임. 패턴이 포함되어 있는 위치 반환*
grep('a', 'bbbb') # 없음
grep('a', 'bbabb') # 뒤에 있는 비교 대상이 한 개 뿐이니까 있다면 위치는 1
grep("pole", c("Equator", "North Pole", "South pole", "poles")) # 3번째와 4번쨰에 있다

# nchar # 문자열의 단어 개수를 계산
nchar(c("South Pole", "한글 문자열", NA))

# substr 함수 # start부터 stop라(include)까지 문자 가져와라
substr("Equator", start = 2, stop = 4)
substr("한글 문자열 추출", start = 2, stop = 4)
substring("한글 문자열 추출", first = 2)
strsplit("6-16-2011", split = "-")
strsplit(c("6-16-2011", "1-1-1-2-1-1"), split = "-") # list로 나온다

# 특수문자 이용한 분리
strsplit("6*16*2011", split = '*') # 이상하다 ==> 이유: 특수문자는 대부분 고유의 기능이 정해져있다
strsplit("6*16*2011", split = '\\*') # 원하던 대로 됨
strsplit("6*16*2011", split = '*', fixed = TRUE) # 같은 기능. 이스케이프 문자를 허용하지 않는다
#### 많이 쓰는 이스케이프 문자  '\t': 탭, '\n':줄바꿈문자. '\d':숫자

list.files()
a = strsplit(list.files(), split = '.', fixed = T) # 파일명과 확장자가 분리되었다
tmp = rep(0, length(a))
for (i in 1:length(a))
{
  b = a[[i]]
  if (length(b) == 2)
  {
    tmp[i] = b[-1]
  }
}
table(tmp) # 확장자의 테이블

regexpr("감자", "맛있는 감자칩") # 문자열내에서 지정한 패턴(문자)과 처음으로 일치한 위치를 알려줌
gsub(pattern = "감자", replacement='고구마',
     x= "머리를 감자마자 감자칩을 먹었다.")  # 대체함


###########################################
# 정규표현식
## OR : |
strsplit('감자, 고구마, 양파 그리고 파이어볼', split = '(, )|( 그리고 )') # |로 or연산 할 때 괄호 써주는 게 에러 줄이는 방법

## 시작 : ^
grep(pattern = '^(감자)', x = '감자는 고구마를 좋아해') # 해당 패턴이 있어서 1
grep(pattern = '^(감자)', x = '고구마는 감자를 좋아해') # 해당 패턴이 없어서 integer(0)

## 끝 : $
grep(pattern = '(좋아해)$', x = '감자는 고구마를 좋아해')

## ANY : [] 안에 있는 것 하나의 문자로 판단
gregexpr(pattern = '[아자차카]', text = '고구마는 감자를 안 좋아해')
gregexpr(pattern = '[(사과)(감자)(양파)]', text = '고구마는 감자를 안 좋아해')

## everything except : [^]
grep(pattern = '^[^(사과)(감자)(양파)]', x = '감자는 고구마를 좋아해') # ^는 시작함을 의미 [^]에서 사과,감자,양파로 시작 안 하는 것을 요구

## abbreviation
### [a-z] 알파벳 소문자 중 아무것이나 1개
### [A-Z] 알파벳 대문자 중 아무것이나 1개
### [0-9] 숫자 문자 중 아무것이나 1개
### [a-zA-Z] 알파벳 소문자나 대문자 중 아무것이나 1개
### [가-힣] 한글중에 아무거나 1개                           [ㄱ-ㅎㅏ-ㅣ가-힣] : 모음, 자음 자체까지 포하
### [^가-힣]
### [:alpha:] 알파벳 문자
### [:lower:] 소문자 알파벳 문자
### [:upper:] 대문자 알파벳 문자
### [:digit:] 숫자
### [:alnum:] 알파벳/숫자
### [:space:] 출력되지 않는 공백 문자(예: carriage return, newline, vertical tab, form feed 등
### [:punct:] 구두점 기호
### [:cntrl:] (출력되지 않는) 컨트롤 문자

## 반복 : {}   {from,to} 를 이용하여 생성. 바로 앞 문자에 대한 패턴임을 주의! o{2,4} 는 문자 o가 2번에서 4번까지 반복되는 패턴
#             [a-z]{3,6} 영어 소문자가 3번에서 6번까지 반복되는 패턴 {5} 는 5번 {5,} 5번 이상 반복을 뜻함.
grep(pattern = '^ab{2,3}', x = 'ab') # ab가 a로 시작하고 b가 2~3번 사이에서 반복 되는가 ==> 아니니까 integer(0)
# *: {0,} # 앞에 것이 여러번 반복되어도 되고 아예 안 사용되어도 됨
# +: {1,} # 앞에 것이 반드시 한 번 이상 반복되어야 한
# ?: {0,1}
# .: 어떠한 문자라도 1개
grep(pattern = '^a+', x = 'ab')
grep(pattern = '^a*', x = 'ab')


# stringr
library(stringr)
hw <- "Hadley Wickham"
str_sub(hw, 1, 6)
strsplit(hw, ' ') # list형 반환
strsplit(hw, ' ')[[1]][2] # 리스트 형의 첫번째에서 두번째 값 가져오기
str_sub(hw, 1, 6)
str_sub(hw, -5) # 거꾸로 가기\
str_sub(hw, -5, -2)
fruits <- c("apples and oranges and pears and bananas",
            "pineapples and mangos and guavas")
str_split(fruits, " and ") # strsplit의 진화형
str_split(fruits, " and ", n = 2) # 2개까지만 분류를 하고 분류하지 않음 (최대개수 설정)
str_split_fixed(fruits, " and ", 4) # 테이블 형태로 만들어주기

fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "^a") # 이 패턴이 존재하는지
r_file <- c("Rscript.exe", "R.exe", "Rcmd.exe")
str_detect(r_file, "^R[A-Za-z0-9]*(\\.exe)$")

str_count(fruit, c("a", "b", "p", "p")) # elementwise count

str_count("pppppppp", "p{1,2}")

shopping_list <- c("apples x4", "flour", "sugar", "milk x2")
str_extract(shopping_list, "\\d") # 결과값 "4" NA  NA  "2"
str_extract_all(shopping_list, "\\d") # 만약 한 element에서 숫자가 연속되면 _all을 붙여야지만 리스트로 여러 값을 출력한다

fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-")
str_replace_all(fruits, "[aeiou]", "-")


# 텍스트 마이닝을 할 때,
# Text를 vector 공간으로 옮겨야 분석이 가능하니까 word embedding을 거쳐야 한다 (categorical인 단어가 numeric화 된다고 볼 수 있다)
# similarity가 높으면 가까이 embedding 되는 것이 합당하다
# 대표 기법: word2vec, fasttext
# 예시
# recipe 사이트: 문서를 단어화 함. 공감/비공감으로 나눠진 평가 scale을 가져온다
# 이런 재료로 분석하면 감성분석이 가능하다






