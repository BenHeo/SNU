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




