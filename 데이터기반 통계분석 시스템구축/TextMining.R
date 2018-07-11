library(tidyverse)
library(httr)
# connection 만들기
con <- url("http://ranking.uos.ac.kr")
read <- readLines(con)
close(con)
str(read)
read[10:20]

# 웹사이트를 허술하게 만들어 놓지 않아서 단순 긁어오는 방법으로는 실무가 어렵다
# get 방식 or post 방식으로 만든다
# ‘get’ 방식으로 읽기: 요청 자료의 형태를 URL 주소 형식으로 전달
# ’post’ 방식으로 읽기: 서버에서 요청하는 form의 형태로 자료를 요청 <- 요청하는 보고서 방식을 알아내서 제출해야 함
# API 에서 요구하는 header 형태 만들기 좋게 httr 사용
# 원하는 노드의 값을 긁어오기 위해 rvest


# 중요한 태그
# <a href => ... < /a> 하이퍼 링크
# <br> 줄바꾸기
# <hr> 가로줄
# <center>...< /center> ...을 가운데 정렬
# <font>...< /font> ...의 폰트를 바꿈
# <ul><li>...<li>...< /ul> ...을 순서없는 목록으로 만듦
# (기본: 까만동그라미)
# <ol><li>...<li>...< /ol> ...을 순서있는 목록으로 만듦
# (기본: 숫자)
# <table>< /table> 표만들기
# <tr>< /tr> 행(<table>...< /table>...에 넣는다)
# <td>< /td> 열(<tr>...< /tr> ...에 넣는다)



# bag of words 는 N-gram의 N이 1인 특수한 형태
# N이 2이면 'the cat', 'cat chased', 'chased the', 'the mouse', 'the dog', 'dog chased' 같이 묶임

# word vector 정규화 : 너무 많이 나오는 the, a 같은 거는 오히려 안 좋다. 멱 형태로 감점을 준다
# idfi = log(N/ni)      (N is the total number of term frequency and ni is that of the ith term frequency.
# wi = tfi × idfi (weighted term frequency).

# similarity 분석 measure
# 코사인 거리 : 문장이 길건 짧건 비율에 의해서만 측정할 때는 오직 각도의 유사도만으로 분석하기 때문에 적합
# 유클리디언 거리 : 긴 문장과 짧은 문장을 다르다고 정의하면 단순 각도의 유사도만으로 하기는 어렵다. 하지만 거리 기반이기에 명확하지 않을 수 있다

