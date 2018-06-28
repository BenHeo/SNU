# Bitwise operation &(and), |(or), !(not)

# if ~~~~} else 를 할 때 else를 }에서 한 칸 띄고(enter) 하지 않고 옆에 쓰는 이유: interpreter언어는 한 줄이 문법적으로 끝나면 코드를 수행한다

# break는 그 loop만 빠져나오지만 stop은 그냥 다 멈춘다
# break, stop 예제를 만들어보자~!!!!!!!!!


# function()
testfunction <- function(x1,x2)
{ 
 v1 = x1^2 + x2
 v2 = x1^2 - x2
 return(c(v1,v2)) # 결과를 여러개 반납
}
testfunction(3, 7)

# 글로벌 변수를 함수에서 참조하지 않게 조심해야 한

# alt 키를 잘 활용해라! alt 왼쪽하면 왼쪽 끝, alt 오른쪽 하면 오른쪽 끝, alt 위 하면 위쪽 줄과 줄바꿈, 아래쪽 하면 아래 줄과 줄바꿈

# 숙제: rowwise average function


# setwd('./fig') 현 폴더의 fig 폴더로 가서 열어라
# setwd('../fig') 현 폴더의 상위폴더에서 fig 폴더로 가서 열어라
# setwd('..../fig') 현 폴더의 상위폴더의 상위폴더에 가서 fig를 열어라



# View(CO2)
# write.csv(CO2, file='CO2.csv', row.names = FALSE)
# write.csv(CO2, file='CO2.csv', row.names = FALSE, sep = ";") # separator를 ;로 한다

# a = read.csv('CO2.csv', header=TRUE)
# read.table("CO2.csv", sep = ";") # 역시 separator를 ;로 한다
# ?read.table 참조    # 특히 nrows 와 skip 중요!
# 
# A = read.table("CO2.dat", header = TRUE, sep = " ", stringsAsFactors = FALSE) # 기본적으로 문자열을 factor로 바꾸지만 그렇게 하지 말아라!
#                                                                                원래 factor로 자동으로 해주는 이유는 할 때마다 integer, character 다 구분하려면 힘들다
# a = read.table("CO2.csv", sep = ";", stringsAsFactors = False, colClasses = c("charactor", "factor", "integer", "factor", "numeric))
# read.table("CO2.csv", nrows = 5)

library(xlsx)
a = list.files()
a[3]
b = list.dirs(recursive = FALSE)
b[1]
getwd()



