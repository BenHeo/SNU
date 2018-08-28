library(ISLR)
library(datasets)
library(tidyverse)

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x)

km.out <- kmeans(x, 2, nstart = 20)
plot(x, col=(km.out$cluster+1),main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)
km.out

set.seed(3)
km <- vector()
for (i in 1:10){
  km.out <- kmeans(x, i, nstart = 10)
  km[i] <- km.out$tot.withinss
}
km
plot(km, type = 'l')
points(km, col = "red", cex = 1.5)
mu_km <- mean(km[2:10])
std_km <- sd(km[2:10])
abline(h=mu_km+2*std_km)
abline(h=mu_km-2*std_km)

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point(size = 2)
irisCluster <- kmeans(iris[,c(3,4)], 3, nstart = 20)
table(iris$Species, irisCluster$cluster)

x = iris[,c(3,4)]
hc.complete <- hclust(dist(x), "complete") # dist is essential
hc.average <- hclust(dist(x), "average") # dist is essential
hc.single <- hclust(dist(x), "single") # dist is essential

plot(hc.complete, main = "Complete Clustering", xlab = "", ylab = "", cex = 0.9)
plot(hc.average, main = "Average Clustering", xlab = "", ylab = "", cex = 0.9)
plot(hc.single, main = "Single Clustering", xlab = "", ylab = "", cex = 0.9)

cutree(hc.complete, 2)
cutree(hc.complete, 3)
hcc_iris <- cutree(hc.complete, 3)
table(iris$Species, hcc_iris)

load("data/AlzheimerDisease/AlzheimerDisease.RData")
head(predictors)
train.x=predictors[1:250,1:129]
test.x=predictors[251:333,1:129]
train.y=as.numeric(diagnosis[1:250])
test.y=as.numeric(diagnosis[251:333])
pr.out <- prcomp(train.x, scale = TRUE)
str(pr.out) # sdev, rotation, center, scale, x
pr.out$rotation
dim(pr.out$x)
str(pr.out$x)
biplot(pr.out, scale = 0, cex = 0.5)
pr.out$sdev[1:10]
pr.var <- pr.out$sdev^2
pr.var[1:10]
pve <- pr.var/sum(pr.var)
pve[1:10]
plot(pve[1:30], xlab="Principal Component",
     ylab="Proportion of Variance Explained", ylim=c(0,0.3),type='b')
plot(pr.out$rotation[,1], pr.out$rotation[,2])


x0=USArrests
set.seed(1)
x5=USArrests[,2]+rnorm(50,0.01)
x=cbind(x0,new = x5)
x=scale(x)
y=1+1*x[,1]+2*x[,2]+3*x[,3]+4*x[,4]+5*x[,5]+rnorm(50)
pr.out=prcomp(x,scale=T)
head(pr.out,n=2)

var_pr <- pr.out$sdev^2
t_var_pr <- sum(var_pr)
explan_prob <- var_pr/t_var_pr
plot(explan_prob, type = 'l')
screeplot(pr.out)

pcscore=pr.out$x[,1:2] #2개의 principal component 사용
load=pr.out$rotation[,1:2]
plm=lm(unlist(y)~pcscore)
pbeta=plm$coef
betaest=rep(0,6)
betaest[1]=pbeta[1]
for(k in (2:6)){
  betaest[k]=pbeta[2]*load[k-1,1]+pbeta[3]*load[k-1,2]
}
for(k in (2:6)){
  betaest[k]=pbeta[2]*load[k-1,1]
}
betaest

library(pls)
set.seed(1)
train = 1:200
test = -train
y.test = Hitters[test, "Salary"]
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,Hitters[test,],ncomp=3)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=3)
summary(pcr.fit)
