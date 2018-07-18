#### exponential
n = 1000
(1+(1/n))^n
exp(1)

x = 1:1500
y = (1+(1/x))^x
plot(y~x, type = 'l')
abline(h = exp(1), col = 'red')

a = 5
x2 = 1:1500
y2 = (1+(a/x2))^x2
plot(y2~x2, type = 'l')
abline(h=exp(a), col = 'blue')

exp(-1 + 0.1*2)
x3 = seq(-1, 1, length.out = 100)
y3 = exp(x3)
plot(y3~x3, type = 'l', ylim = c(0, 5))

#### matrix
X = matrix(runif(15), 5, 3)
X[3,2]
X[, 2, drop=F]
X[4, , drop=F]
Xt = t(X)
X2 = Xt%*%X
Xinv = solve(X2) # 역행렬
Xinv %*% X2 # == I
X2 %*% Xinv # == I

p = 5
n = 4
a = matrix(runif(5), p, 1)
X = matrix(runif(20), n, p)
t(a) %*% a
t(X) %*% X
solve(t(X) %*% X)
t(a)%*%solve(t(X) %*% X)%*%a

mean_mat <- matrix(c(0, 1, -1))
cov_mat <- matrix(c(1, 0.5, 0, 0.5, 1, 0.3, 0, 0.3, 1), 3, 3)
X = matrix(c(1, 0, 1/2))
gX = exp((t(X-mean_mat)%*%solve(t(cov_mat))%*%(X-mean_mat))/-2) # X가 주어졌을 때 다변량정규분포의 pdf

# 포아송 분포
px = seq(0, 10, length.out = 1000)
py = ppois(px, lambda = 1)
py2 = ppois(px, lambda = 2)
plot(py~px, type = 'l')
plot(py~px, type = 's')
abline(h = 1, col = 'red', lty = 2)
lines(px, py2, col = 'green')


## 중심 극한 정리
n = 10000
z = rexp(n)
x = c()
for (i in 1:1000)
{
  idx = sample(1:n, 25)
  x[i] = mean(z[idx])
}
idx
hist(x)



## 감마 분포
gamma(3.1)
gamma(2)
gamma(3)
gamma(4)
gamma(5)
gamma(6) # 정수 감마는 팩토리얼

# plot gamma dist.
alphag = 2
alphag2 = 8
betag = 0.5
xg = seq(0, 10, length.out = 100)
yg = dgamma(xg, alphag, rate = 1/betag)
yg2 = dgamma(xg, alphag2, rate = 1/betag)
plot(xg, yg, type = 'l')
lines(xg, yg2, type = 'l', col = 'red')


# Bern MLE estimation
sampl_B <- c(1,1,0,1,1)
thetaNow <- sum(sampl_B)/length(sampl_B)
dbinom(sampl_B, size = 1, prob = 0.6, log = FALSE)
loglike <- like <- c() # make null vector
theta.vec <- seq(0,1, length.out = 100)
for (i in 1:100)
{
  theta <- theta.vec[i]
  like[i] <- prod(dbinom(sampl_B, size = 1, prob = theta, log = FALSE)) # multiply all vectors in it
  loglike[i] <- sum(dbinom(sampl_B, size = 1, prob = theta, log = TRUE))
}
plot(theta.vec, like, type = 'l', col = 'blue')
plot(theta.vec, loglike, type = 'l', col = 'red')


# Norm MLE estimation given sigma
sampl_N <- c(0.1, 0.5, 0.3, 0.15, 0.2)
loglike <- like <- c()
theta.vec <- seq(0, 1, length.out = 100)
for (i in 1:100)
{
  theta <- theta.vec[i]
  like[i] <- prod(dnorm(sampl_N, theta, 1, log = FALSE))
  loglike[i] <- sum(dnorm(sampl_N, theta, 1, log = TRUE)) # use sum in log forms
}
plot(theta.vec, like, type = 'l', col = 'blue')
plot(theta.vec, loglike, type = 'l', col = 'red')


# Norm MLE estimation mu and sigma ungiven
sigma.vec <- seq(0.1, 0.3, length.out = 100)
mu.vec <- seq(0, 1, length.out = 100)
simu <- matrix(0, 100, 100)
logsimu <- matrix(0, 100, 100)
for (i in 1:100)
{
  for (j in 1:100)
  {
    simu[i, j] = prod(dnorm(sampl_N, mu.vec[i], sigma.vec[j], log = FALSE), na.rm = TRUE)
    logsimu[i, j] = sum(dnorm(sampl_N, mu.vec[i], sigma.vec[j], log = TRUE), na.rm = TRUE)
  }
}
filled.contour(mu.vec, sigma.vec, simu, nlevels = 30, col = heat.colors(30))
filled.contour(mu.vec, sigma.vec, logsimu, nlevels = 30, col = heat.colors(30))


# logistic
z = seq(-10, 10, length.out = 1000)
y = exp(z)/(1+exp(z))
plot(z,y, type = 'l')
