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








# Lasso Bootstrap

library(glmnet)
Hitters <- na.omit(Hitters)
dim(Hitters)

attach(Hitters)
x = model.matrix(Salary~., Hitters)
y = Hitters$Salary
n = 263
B = 1000
best = matrix(0, B, 21) # 원래 20개로 했는데 Beta0 이 있어서 1개 추가해줌

for (b in (1:B)){
  bid = sample(n,n, replace = TRUE)
  bx = x[bid,]
  by = y[bid]
  grid = 10^seq(4, -1, length.out = 100)
  cv.out = cv.glmnet(bx, by, alpha = 1, lambda = grid) # cross validation 결과
  blamb = cv.out$lambda.min # 결과가 가장 작은 걸 선택
  lasso.mod = glmnet(bx,by, alpha = 1, lambda = exp(blamb))
  best[b,] = as.vector(coef(lasso.mod))
  cat("\t b=")
  cat(b)
}
best
mu <- apply(best, 2, mean)
se <- apply(best, 2, sd)
 
tstat <- mu/se
pvalue <- 2*(1-pnorm(abs(tstat)))
pvalue
select <- (best != 0)
stab <- apply(select, 2, sum)/B
numselect <- apply(select, 1, sum)
hist(numselect, breaks = 20)
