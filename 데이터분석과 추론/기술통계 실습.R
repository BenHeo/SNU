VADeaths
barplot(VADeaths, beside = TRUE,col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk"),legend = rownames(VADeaths), ylim = c(0, 100))
title(main = "Death Rates in Virginia", font.main = 4)

#원그래프
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry","Apple", "Boston Cream", "Other", "Vanilla Cream")

#기본
pie(pie.sales)
title(main = "Sales", font.main = 4)

#원그래프 크기, 방향
pie(pie.sales, radius=1, clockwise=T)
title(main = "Sales", font.main = 4)

#연속형
#히스토그램
data("faithful")
x<-faithful$eruptions
hist(x)

hist(x, breaks=20) # 항상 x축을 어떻게 나누느냐가 중요
hist(x, breaks=20, freq=FALSE)

#계급구간 길이의 중요
par(mfrow=c(1,2))
edge1<-seq(from=1,to=6,by=0.4)
edge2<-seq(from=1,to=6,by=1)
hist(x,breaks=edge1,freq=F,xlim=c(0,6),ylim=c(0,0.6),main="h=0.4")
hist(x,breaks=edge2,freq=F,xlim=c(0,6),ylim=c(0,0.6),main="h=1")
#줄기-잎 그림
stem(faithful$eruptions)

#산점도
plot(iris$Petal.Length,iris$Petal.Width,xlab='Sepal.Length',
     ylab='Sepal.Width',cex.lab=1,cex.axis=1,type='n',cex=2)
points(iris$Petal.Length[iris$Species=='setosa'],
       iris$Petal.Width[iris$Species=='setosa'],col='red',lwd=2)
points(iris$Petal.Length[iris$Species=='versicolor'],
       iris$Petal.Width[iris$Species=='versicolor'],col='blue',lwd=2)
points(iris$Petal.Length[iris$Species=='virginica'],
       iris$Petal.Width[iris$Species=='virginica'],col='green',lwd=2)

#기술통계
#평균 분산
n=length(faithful$eruptions)
sum((faithful$eruptions-mean(faithful$eruptions))^{2})/(n-1)
var(faithful$eruptions) # n-1로 해줌

sqrt(var(faithful$eruptions))
sd(faithful$eruptions)

#분위수
pquant=quantile(faithful$eruptions,probs=c(0.25,0.5,0.75))
pquant
pquant[3]-pquant[1]
IQR(faithful$eruptions)

max(faithful$eruptions)-min(faithful$eruptions)
rfaithful=range(faithful$eruptions)
rfaithful[2]-rfaithful[1]
#outlier detection
iqr.val=IQR(faithful$eruptions)
c(pquant[1]-1.5*iqr.val, pquant[3] +1.5*iqr.val)

faithful$eruptions[faithful$eruptions > pquant[3] +1.5*iqr.val]
faithful$eruptions[faithful$eruptions < pquant[1] -1.5*iqr.val]

summary(faithful$eruptions)
apply(faithful, 2, summary) # summary for each col

#Boxplot
par(mfrow=c(1,2))
boxplot(faithful$eruptions,main='Eruptions')
boxplot(faithful$waiting,main='Waiting')

#왜도, 첨도
xvec=seq(0.01,0.99,0.01)
par(mfrow=c(1,2))
plot(xvec,dbeta(xvec,2,5),type='l',lwd=2,xlab='',ylab='')
plot(xvec,dbeta(xvec,7,2),type='l',lwd=2,xlab='',ylab='')
x1= rbeta(1000, 2, 5)
x2= rbeta(1000, 7, 2)
(sum((x1-mean(x1))^3)/length(x1))/(var(x1))^{3/2}
(sum((x2-mean(x2))^3)/length(x2))/(var(x2))^{3/2}
par(mfrow=c(1,1))
xvec=seq(-4,4,0.01)
plot(xvec,dnorm(xvec,0,1),type='l',lwd=2,xlab='',ylab='', main="Normal and t-distribution")
lines(xvec,dt(xvec,2),type='l',lwd=2,lty=2, col='red')
x1= rt(1000, 2)
(sum((x1-mean(x1))^4)/length(x1))/(var(x1))^{2} -3 # 정규분포의 첨도 (첨도란 꼬리가 두꺼운 정도) # outlier 판단에 도움

#이변량
x= faithful$eruptions; y= faithful$waiting
cov(x,y)/(sd(x)*sd(y))
cor(x,y)
plot(x,y,xlab='',ylab='')

#원그래프에 숫자 표시
piepercent<- round(100*pie.sales/sum(pie.sales), 1)
pie(x=pie.sales, labels=piepercent,col=rainbow(length(pie.sales)))
title(main = "Sales", font.main = 4)
legend('topright', names(pie.sales), cex = 0.7, fill=rainbow(length(pie.sales)))
