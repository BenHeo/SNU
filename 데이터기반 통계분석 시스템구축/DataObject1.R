# r의 elementwise operation 가능 확인
x <- c(1,2,3)
y <- c(1,2)
x/y
x^2
x%/%2  #나머지
x%%2  #나머지

x <= 1



# 
set.seed(1)
x <- runif(100)
x[1]
x[x>0.8]
length(x[x>0.5]) # 0.5 이상이 몇개?
length(x[x>0.5])/100 # 0.5 이상이 몇퍼센트?
x <- c(3,4,1,2)
y <- x[x>5];y # y에 name space를 할당해 주었지만 값은 없으므로 numeric(0)

# which
which(x>5)
which(x>2)
x[which(x>2)]

#NA
x = c(1:4, NA, 5:8)
x <= 4 # NA는 모든 연산은 NA로 반환함
x[x<=4] # 단순 TRUE FALSE 만 하면 NA도 나와버림
x[which(x<=4)] # which 사용하면 안 나옴
# NA == NA # 지들끼리 같냐고 물어도 NA
is.na(NA)
sum(c(1:5, NA), na.rm = TRUE)

# %in% function
1 %in% c(1,2,4,1)
x <- c(3,1,4,1)
x[x %in% c(2,4,3)]

# match
match(1, c(2,1,4))
match(c(1,4), c(2,-1,1,4)) # 찾으면 끝이기 때문에 distinct한 값을 가지는 것들만 활용

# unique
x <- rep(c("a", "b"), 4)
length(unique(x)) == length(x) # FALSE 즉 중복이 있다


# matrix
y <- matrix(c(1,2,3,4), nrow = 2); y
y <- matrix(c(1,2,3,4), nrow = 2, byrow = T); y # byrow = TRUE 이면 로우 기준으로 먼저 채움
y[1:2,]
y[2,]
y[-2,]
class(y)
class(y) == "matrix"
dim(y); ncol(y); nrow(y)
class(y[1,]) # 자동으로 numeric으로 변경
class(y[1,,drop=FALSE]) # 자동으로 바꾸지 마

# 반복적으로 늘리는 테크닉
a = NULL # a를 할당하기 위함
a = c(a, "init");a

a = 0
a[10] <- 22;a

#
x <- 1:10
typeof(x[1])
x[1] <- "a"
typeof(x[1])
typeof(x[2]) # 벡터는 관리하는 값들이 일관적이여야 하기 때문에 바뀌었다

# dataframe 쓰는 이유 == 여러 형태의 데이터들을 다루고 싶다
kids <- c("Jack", "Jill")
ages = c(12,10)
d = data.frame(kids, ages, stringsAsFactors = F); d
str(d)
# d = data.frame(x1=kids, x2=ages, stringsAsFactors = F); d # 이름 붙이기
class(d[,1]) # character
class(d[1,]) # data.frame
d$ages # 가독성 good
names(d) # dataframe의 name들을 알려줘

A = data.frame(x1 = rep(0,10), x2 = rep('b', 10))
B = data.frame(x2 = rep(0,10), x3 = rep('d', 10))
AB = cbind(A,B)
head(AB)
BA = rbind(A,B) # doesn't work, because diff col name
A = data.frame(x1 = rep('b',10), x2 = rep(0, 10))
B = data.frame(x2 = rep(0,10), x1 = rep('d', 10))
BA = rbind(A,B) # x1, x2 이름에 맞춰 붙는다. 그것들끼리 type도 맞아야 한다


# list
## 리스트는 chain으로 이해하면 좋다 obj1 - obj2 - obj3 - obj4 ...
## list obj는 어떤 형태도 된다. vector, matrix, array, data.frame, even list
## 실험을 한다고 했을 때, 처음 실험에서 나온 여러 결과들을 obj1에 삽입, 두번째에서 나온 걸 obj2에 삽입... 식으로 하면 분석가 입장에서 좋다
## 그리고 길이가 달라도 저장이 된다. Because of chaining

# 빈 list
jini <- vector(mode = 'list', length = 10); jini
# j$s  가장 가독성 좋은 기능
# j[["s"]] 위와 같은 기능
# j["s"] 이렇게 쓰는 것은 자체보다 j[c("s","v")] 처럼 서브 리스트 만들 때 좋다
j <- list(names = c('joe', 'nick'), salary = matrix(55000:55003, 2, 2), union = TRUE)
j[[1]]
j[[1]][2] # find nick
j$salary[2] # matrix에 그리 적합한 코드가 아님... 애매하자나
j$salary[,2]

j$history # 설정 안 해줬으니 NULL
j$history <- 1:10; j$history

# j$salary <- NULL 이걸 하면 salary 부분이 없어질 것이다. 이 때 체이닝상 salary는 2번이었는데 이제 union이 2가 된다

############ data.frame 은 list 이면서 matrix의 인덱싱 기능이 가능하다
d$add <- "a"
d
d$kids <- NULL ; d


# factor
## factor 는 정수다
a = c("jack", "jill", "nick", "richard")
af = factor(a)
str(af) # 1,2,3,4라는 level이 reference로 나온다
levels(af)

as.numeric(af) # 사실은 숫자로 봐도 됐던거임
# factor 활용법
# factor를 통해 slicing 하는 것이 가능하다 a[cyl_factor]

# levels 통해 실재로는 없는 데이터도 고려해서 구성할 수 있다
cand =  c("jack", "jill", "nick", "richard")
a =  c("jack", "nick", "richard","jack","richard")
table(a)
af = factor(a, levels = cand)
table(af)

