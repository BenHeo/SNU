x <- c("M","F","F","I","M","M","F")
x
labels(x,c("F","M","I"))
f <- (x=="F")    # True, False 가 나뉘므로 인덱스 기억하는 형태임
m <- (x=="M")
x[f] = "M"
x[m] = "F"
x


######################################

data(iris)
str(iris)
summary(iris)
plot(iris$Sepal.Length, iris$Petal.Length, xlim=c(2,8), ylim = c(0,8)) # xlim, ylim을 활용해서 범위를 정해준다
plot(iris$Sepal.Length, iris$Petal.Length, xlab = "Sepal length", ylab = "Petal length") # xlab, ylab을 활용해서 이름을 정해준다
plot(iris$Sepal.Length, iris$Petal.Length, main = "Sepal vs. Petal length") # main을 통해 전체 그림의 타이틀을 정해준다
plot(iris$Sepal.Length, iris$Petal.Length, cex=2)  # cex 조절은 점의 크기를 바꿔준다
plot(iris$Sepal.Length, iris$Petal.Length, pch=2, cex = 2) # pch로 모양을 바꿨다
plot(iris$Sepal.Length, iris$Petal.Length, col = 2) # color를 바꿔봤다
plot(iris$Sepal.Length, iris$Petal.Length, type = 'l') # type을 l로 바꿔서 line을 만들었다

# par() 통해서 그래프를 여러방면으로 직접 조정 가능하다
par(mfrow = c(2,2))
plot(iris$Pepal.Length, ylab = "", main="Petal Length")
plot(iris$Sepal.Length, type = 'l', ylab = "", main = "Sepal Length")
plot(iris$Sepal.Length, type = 'l', lwd = 3, col = 2, ylab = "", main = "Sepal Length")  # line의 굵기 조정을 위해 lwd 사용
plot(iris$Sepal.Length, col=2, ylab = "", main = "Sepal Length")
par(mfrow = c(1,1)) # 원상복귀

# 범례###############################
plot(iris$Sepal.Length, iris$Sepal.Width, xlab = "Sepal Length", ylab = "Sepal Width", pch=16)
points(iris$Petal.Length, iris$Petal.Width, col=2)
legend("bottomright", legend = c("Sepal", "Petal"), pch=c(16,1), col=1:2)


# 히스토그램##################
hist(iris$Sepal.Length, breaks = 20, freq = FALSE) # 20개 구간으로 나누고 frequency 기반이 아니라 density 기반으로 표시해달라는 의미