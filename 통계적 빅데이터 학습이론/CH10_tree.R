library(ISLR)
library(tree)
attach(Carseats)
High = ifelse(Sales <= 8, "No", "Yes")
carseat2 <- data.frame(Carseats, High)
detach(Carseats)
tree.carseat <- tree(High~.-Sales, carseat2)
summary(tree.carseat)
plot(tree.carseat)
text(tree.carseat, pretty=0, cex=0.8) # pretty = 1 makes texts abbreviated
tree.carseat

# use
set.seed(2)
train=sample(1:nrow(carseat2), 200)
carseats.test=carseat2[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,carseat2,subset=train)
tree.pred=predict(tree.carseats,carseats.test,type="class")
table(tree.pred,High.test)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats) # "size" "dev" "k" "method"
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
par(mfrow=c(1,1))
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0,cex=0.8)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
