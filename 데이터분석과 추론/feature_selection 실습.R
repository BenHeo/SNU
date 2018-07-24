data(USArrests)
head(USArrests)
arrest <- lm(Murder~., USArrests)
# 회귀직선이 유의미한지 알려면 B1 = B2 = B3 = ... = 0 이다를 귀무가설로 둔다
# 이 때 F분포는 MSR/MSE ~ F(k, n-k-1)이다
anovar <- anova(arrest)
anovar$`Pr(>F)`
summaryArrest <- summary(arrest)
sse <- sum(summaryArrest$residuals^2)
sse/(length(arrest)-2) 

plot(arrest)

step(arrest, direction = "forward")
step(arrest, direction = "backward")
step(arrest, direction = "both")
