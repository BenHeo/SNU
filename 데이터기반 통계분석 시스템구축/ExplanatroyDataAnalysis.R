# 범주형 변수의 요약치는 빈도, 상대빈도이고 시각화는 이것들에 대해 하는 것

state.region
counts <- table(state.region)
counts
barplot(counts, main = "simple bar chart", xlab = "region", ylab = "freq")
barplot(counts/sum(counts), main = "simple bar chart", xlab = "region", ylab = "freq")
cylt <- table(mtcars$cyl)
cylt
cyl.name <- c("4 cyl", "6 cyl", "8 cyl")
barplot(cylt/sum(cylt), main = "freq of cyl", xlab = "cyl", ylab = "freq", names.arg = cyl.name)

# pie plot은 좋다고 할 수는 없다
(rel.cyl <- cylt/sum(cylt))
rel.cyl <- round(rel.cylt)
sum(rel.cyl)

# if ((sum(rel.cyl)-1) != 0)
# {
#   d = sum(rel.cyl) - 1
#   rel.cyl[which.max(rel.cyl)] = rel.cyl[which.max(rel.cyl)] -d
# }  # 반올림 때문에 합이 1보다 커졌을 경우는 이렇게 해라

pie(rel.cyl)
cyl.name2 <- paste0(names(rel.cyl), " cyl (", round((rel.cyl)*100, 2), "%)")
pie(rel.cyl, labels = cyl.name2)

library(plotrix)
# pieplot의 크기 구분 모호함 해결 위해 겹쳐서 표현
fan.plot(rel.cyl, labels=cyl.name2)


library(vcd)
head(Arthritis, n = 3) #류마티스 관절염
my.table <- xtabs(~ Treatment + Improved, data = Arthritis) # table보다 직관적인 xtabs
my.table
barplot(my.table, col = c("green", "red")) # 원하는 것을 그린 것이 아니라 원하는 것에 대해 표현이 잘 안 되게 잘못 그린 그림
barplot(t(my.table), col = c("green", "red", "orange"))
# or 
me.table <- xtabs(~ Improved+Treatment, data = Arthritis)
barplot(me.table, col = c("green", "red", "orange"))
# me.table[,1] <- me.table[,1]/sum(me.table[,1])
# me.table[,2] <- me.table[,2]/sum(me.table[,2])
a <- colSums(me.table)
me.table/rep(a, each = 3)
sweep(me.table, 2, a, FUN = "/")

# 부모님 안전벨트 착용유무와 자녀 착용유무 비교
tmp <- c("buckled", "unbuckled")
belt <- matrix( c(58, 2, 8, 16), ncol = 2, 
                dimnames = list(parent = tmp, child = tmp))
belt
# spine() 함수는 두 막대의 크기를 같게 만들어 비율의 차이를 비교하기 쉽게 할 수 있다.
spine(belt, gp = gpar(fill = c("green", "red")))


# 연속형 자료 요약치 평균, 중앙값, 분위수
x = rnorm(100)
boxplot(x, col = "lightblue")

# 박스플롯으로 안 되고 히스토그램 그려야 하는 경우
x = faithful$waiting
boxplot(x) # bimodal 인지 모르겠다
hist(x, nclass = 8) # bimodal 인 걸 알겠다
# 히스토그램의 형태는 계급구간을 어떻게 설정하는가에 따라 달라진다.
# 주로 nclass 에 개수를 입력하여 계급구간의 설정한다.      # x의 수가 많아지면 nclass도 늘려줘야 확률밀도함수 추정에 좋다
# 정확한 계급구간은 break 옵션을 통해 정의할 수 있다.
# 히스토그램의 높이를 확률로 표시하기 위해서 probability=T를 사용한다.

# 히스토그램을 그리는 방법 중 구간을 나누고 점이 가까우면 데이터로 치는 비중이 높아지고 멀면 비중이 낮아지게 해서 하는 방법을 kernal smoothing이라 한다
hist(x, nclass = 10, probability = T)
lines(density(x), col="red", lwd = 2) # kernal smoothing
lines(density(x, bw = 15), col="lightblue", lwd = 2) # bandwidth increased


# violin plot
library(vioplot)
x = rpois(1000, lambda = 3)
vioplot(x, col = "lightblue")

attach(mtcars)
# multi plot
par(mfrow = c(3,1))     # 밑에 lim 들이 통일되어 있다는 것을 주
hist(mpg[cyl==4], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40),  ylim = c(0,10), col = 'lightblue',
     nclass = trunc(sqrt(length(mpg[cyl==4]))))
hist(mpg[cyl==6], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40),  ylim = c(0,10), col = 'orange',
     nclass = trunc(sqrt(length(mpg[cyl==6]))))
hist(mpg[cyl==8], xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40),  ylim = c(0,10), col = 'red',
     nclass = trunc(sqrt(length(mpg[cyl==8]))))
par(mfrow = c(1,1))

# line으로 겹쳐 그리기
plot(density(mpg[cyl==4]), xlab="MPG", main = "MPG dist by cylinder",
     xlim = c(5, 40), ylim = c(0.,0.26))
lines(density(mpg[cyl==6]), col = "red", lty = 2)
lines(density(mpg[cyl==8]), col = "blue", lty = 3)      
legend("topright", paste(c(4,6,8), "Cylinder"),
       col = c("black","red", "blue"),
       lty = c(1,2,3), lwd = 3, bty ="n")



### Color
head(colors())
mycol = colors()
plot(1:80, y = rep(1,80), col = mycol[1:80], cex = 2, pch = 20, ylim=c(0,1))
points(1:80, y= rep(0.5, 80), col = mycol[81:160], cex= 2, pch = 20 )
points(1:80, y=rep(0,80), col = mycol[161:240], cex = 2, pch = 20 )

rgb(10, 4, 23, maxColorValue = 255, alpha = 10)
col2rgb('lightblue')

# rgb는 어렵다
hcl(h = 0, c = 35, l = 85, alpha = 0.1)

# 어차피 내가 만드는 건 예쁘기 만들기 어렵다... 만든 palette 쓰자
heat.colors(4, alpha = 1)
rev(heat.colors(4, alpha = 1))

x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = topo.colors(20, alpha = 1), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = 'white')

image(x, y, volcano, col = heat.colors(20, alpha = 1), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = 'white')
# 거꾸로 하고 싶다면?
image(x, y, volcano, col = rev(heat.colors(20, alpha = 1)), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = 'white')

# r에서 제공하는 palette 사용
library(RColorBrewer)
brewer.pal(5, "Blues")
display.brewer.all()
library(colorspace)
# pal = choose_palette() # new window pop up to select palette
# my_col = pal(50)
# my_col