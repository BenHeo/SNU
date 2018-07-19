rn <- rnorm(200)
rn
mean(rn)
sd(rn)
summary(rn)
hist(rn, xlim = c(-3,3))
boxplot(rn)
abline(h=0)
sum(rn>0&rn<=1)
sum(rn>1 & rn <= 2) # ***그림을 그렸을 때 비슷하게 나온다는 의미는 임의의 구간에 들어갈 상대빈도가 비슷하다는 것***

rg <- rgamma(200, 2, 8)
rg
summary(rg)
sd(rg)
hist(rg)
boxplot(rg)
sum(x>1 & x<=2)
sum(x>3 & x<=5)

for (i in 1:3)
{
  n = 100^i
  rg2 = rgamma(n, 2, 1)
  hist(rg2, nclass = sqrt(n), probability = TRUE) # 갈수록 랜덤했던 분포가 점점 더 비슷해진다
  print(mean((rg2>3) & (rg2 <=5)))
}

n = 100
rg2 = rgamma(n, 2, 1)
mean((rg2>=0) & (rg2 <=2))
mean((rg2>=0) & (rg2 <=3))
mean((rg2>=0) & (rg2 <=5))
cdf_gamma = c()
z <- seq(0, 10, length.out = 1000)
for (i in 1:1000){
  cdf_gamma[i] <- mean((rg2>=-Inf) & (rg2<z[i]))
}
# cdf_gamma
plot(z, cdf_gamma) # alpha = 2, beta = 1

rg3 = rgamma(n, 2, 8)
cdf_gamma2 = c()
for (i in 1:1000){
  cdf_gamma2[i] <- mean((rg3>=-Inf) & (rg3<z[i]))
}
# cdf_gamma
lines(z, cdf_gamma2, col = 'red', lwd = 5) # alpha = 2, beta = 8


# mu 0 sigma 2 일 때 데이터가 2 안에 있을 확률
pnorm(2, 0, 2)
# 2와 1 사이에 있을 확률
pnorm(2, 0, 2)-pnorm(1, 0, 2)
pgamma(2, 1, 1) - pgamma(1, 1, 1)
# 2보다 작을 확률
1 - pgamma(2, 1, 1)


x = rnorm(200)
y = rnorm(200)
xy = x*y
hist(xy)
xx = x^2 # 카이스퀘어
hist(xx)


# 회귀모형에 대한 생각
x1 = rgamma(200, 2, 1)
x2 = rgamma(200, 2, 1)
ep = rnorm(200) # epsilon
y = 1 + x1 + ep # x1에 대해서 y 값이 얻어진다. x2는 실제로는 상관없는 데이터
plot(y~x1)
plot(y~x2)

n = 200
p = 10
x = matrix(rgamma(n*p, 1, 2), n, p); dim(x)
b = rep(0, p)
b[3:4] = c(1.5, -1)
y = 1 + x%*%b + rnorm(n)
plot(x[,3], y)
plot(x[,4], y)
plot(x[,1], y) # seems very uncorrelated

# lm function
df <- data.frame(y = y, x = x)
# 전진법 : 한 번 좋다고 여겨져서 들어가면 빠지지 않는다
lm(y~1, df) # regression y = B0 (constant)
lm(y~x.1, df)
lm(y~x.3, df)
lm(y~x.4, df)
lm(y~x.1+x.2, df)
lm(y~x.1+x.2+x.3, df)

sum(lm(y~1, df)$residuals^2)
sum(lm(y~1+x.1, df)$residuals^2)
sum(lm(y~1+x.1+x.2, df)$residuals^2)
sum(lm(y~1+x.1+x.2+x.5, df)$residuals^2)
sum(lm(y~1+x.1+x.2+x.5+x.6, df)$residuals^2) # 쓸데없는 변수들을 추가해도 linear model의 효율은 무조건 조금이라도 좋아진다
sum(lm(y~1+x.3, df)$residuals^2) # with one variable x.3 is most efficient
sum(lm(y~1+x.4, df)$residuals^2) # x.4 is second efficient
sum(lm(y~1+x.3 + x.1, df)$residuals^2)
sum(lm(y~1+x.3 + x.2, df)$residuals^2)
sum(lm(y~1+x.3 + x.10, df)$residuals^2)
sum(lm(y~1+x.3 + x.4, df)$residuals^2)
sum(lm(y~1+., df)$residuals^2) # all variables considered

lm_fit <- lm(y~x.3+x.4, df) # best sub_model(부모형)
lm_fit
str(lm_fit)
sum(lm_fit$residuals^2) # Sum of Squares

# 어떤 변수가 모집단의 생김새를 가장 비슷하게 맞추는지 알고 싶다
## 훈련 셋과 검증 셋을 만들자
n_test = 100
p_test = 10
x_test = matrix(rgamma(n_test*p_test, 1, 2), n_test, p_test); dim(x)
y_test = 1 + x_test%*%b + rnorm(n_test)
fit0 = lm(y~1, df)
fit1 = lm(y~1+x.4, df)
fit2 = lm(y~1+x.4+x.3, df)
fit3 = lm(y~1+x.4+x.3+x.8, df)
test_df <- data.frame(y=y_test, x=x_test)
real_answer <- test_df$y
sum((predict(fit0, test_df)-real_answer)^2)
sum((predict(fit1, test_df)-real_answer)^2)
sum((predict(fit2, test_df)-real_answer)^2)
sum((predict(fit3, test_df)-real_answer)^2) # worse quality

# AIC BIC CV 등등이 있다

