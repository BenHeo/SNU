library(leaps)
library(ISLR)

head(Hitters)
regfit.full <- regsubsets(Salary~., Hitters) # 진짜 신기한 기능 p = 1~p`까지 기준으로 알아서 최적 변수 p개를 찾아준다
summary(regfit.full)


# forward
regfit.full <- regsubsets(Salary~., Hitters, nvmax = 19, method = 'forward')
reg.summary <- summary(regfit.full)
names(reg.summary)
oldpar <- par()
par(mfrow = c(2,2))
plot(reg.summary$rss)
plot(reg.summary$adjr2)
argmax_adjr2 <- which.max(reg.summary$adjr2)
points(argmax_adjr2, reg.summary$adjr2[argmax_adjr2], col = "red", pch = 12, cex = 3)
plot(reg.summary$cp)
plot(reg.summary$bic)
