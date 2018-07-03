data(mtcars)
# View(mtcars)
str(mtcars)
names(mtcars)
plot(mpg~disp, mtcars) # ~는 formula y = ax 에서 y가 앞에 있고 x가 뒤에 있게 하는 것 같은 원리
a = "mpg~disp"
a_f = as.formula(a); class(a_f)
plot(a_f, mtcars)
?plot
plot(hp~disp, mtcars) # hp = B0 + (B1 * disp) + error(평균이 0)

# 회귀분석은 등분산 가정을 지켜야 하는데 이분산 가정이 되는 경우 예측이 힘들겠다고 생각하면 됨
# 에러텀이 0 인 경우를 생각해서 잇는 것과 상위 10% 혹은 5%를 생각해서 잇는 경우가 기울기가 다르다면 등분산성이 어긋난다

set.seed(1)
x = rnorm(100)
y = 2 + 2*x + rnorm(100)
plot(y~x, main = "y=2x+2") # or plot(x,y)

# plot types : p(point), l(line), b(both point and line), s(step), n(no plot)
x = seq(-2, 2, length.out = 10)
y = x^2
plot(x, y, type = 'p')
plot(x, y, type = 'l')
plot(x, y, type = 'b')
plot(x, y, type = 's')
plot(x, y, type = 'n')
plot(x, y, type = 'b', lty = 3) # different line type
plot(x, y, type = 'b', pch = 2) # different shape

plot(x=1:25, y=rep(0,25), pch=1:25) # kyakyakyakya
head(colors()) # colors in r pallete

plot(x,y, type="b", xlab="xx", ylab="yy", main="y=x^2", col="lightblue")
plot(x,y, type="b", xlim= c(-1,1)) 

# draw multiple plots at once
plot(~mpg+disp+drat, mtcars, main="Simple Scatterplot Matrix", col = "orange", pch = 19)


plot(x,y, pch =20, main="scatter plot")
abline(a=1, b=2, col="red") # a + bx
abline(v=1, col="blue") # vertical line
abline(h=1, col="green") # horizontal line

plot(x=1,y=1, type='n', xlim=c(0,10), ylim=c(0,5), xlab = 'time', ylab = '# of visiting')
x = 0:10
set.seed(1)
y=rpois(length(x), lambda=1)
points(x,y,col="blue", type="s")
points(x,y,col="red", type="l", lty = 3)


plot(0,0, type='n', xlim=c(-2,2), ylim=c(-2,2))
x = c(-2,1,0,1,0)
y = c(0,-1,2,-2,1)
lines(x,y) # please draw by order :(
# NA is used for disconnect line
plot(0,0, type='n', xlim=c(-2,2), ylim=c(-2,2))
x = c(-2,1,NA,1,0)
y = c(0,-1,NA,-2,1)
lines(x,y) # still not good
# use group or order


plot(0,0, type='n', xlim=c(1,5), ylim=c(0,2))
x = seq(1,5,1)
abline(v=x, lty=1:length(x))

z = sort(rnorm(100))
y1 = 2+ z + rnorm(100)
plot(z, y1, col="blue", pch=3)
points(z, y1/2, col="red", pch=19)
legend("topright", c("pch_3", "pch_19"), col=c("blue", "red"), pch = c(3,19))




### Visualization of KNN

set.seed(1)
x <- sort(rnorm(100))
y <- 3 + x^2 + rnorm(100)
plot(x, y, pch = 20)
fit = lm(y~x)
str(fit)
coef <- fit$coefficients
coef[1]
coef[2]
abline(coef[1], coef[2], col='red') # model bias ==> evaluated by least square ===> enlarger model space

# y_hat(x) = 1/k * sum(index set of xi k-nearest to x * yi)
# KNN is non-parametric regression which means KNN doesn't assume model space
library(FNN)
k10zero <- knnx.index(x, 0, k=10)
x[47]
x[46]
idx <- k10zero[1,]
points(x[idx], y[idx], pch = 19, col = 'green' )
abline(v=0, lty = 3) 
k10mean0 <- mean(y[idx])
abline(h=k10mean0, col = 'blue')

eval.n = 100
eval.point = seq(-3,3,length.out = 100)
plot(x,y,pch=20)
idx.mat <- knnx.index(x, eval.point, k=10)
yhat <- rep(0, eval.n)
for (i in 1:eval.n){
  yhat[i] <- mean(y[idx.mat[i,]])
}
lines(eval.point, yhat, type = 'l', col = 'red')




a = matrix(1:25, 5, 5)
image(a)
a

z <- 2*volcano
dim(z)
x <- 10*(1:nrow(z))
y <- 10*(1:ncol(z))
z[30,4]
x[30]
y[4]
persp(x,y,z, theta = 135, # 산 모양
      ltheta = 20, col = "green3")
contour(x,y,z) # 등고선
