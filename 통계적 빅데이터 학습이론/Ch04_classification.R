library(ggplot2)
ing = read.csv("data/insurance/insurance.csv", header = T)
ing = ing[,-c(3,4)]
ing$clm = as.factor(ing$clm)
head(ing)
ggplot(data = ing, aes(exposure, veh_value)) + geom_point(alpha = 0.1, aes(color = clm))
load("data/AlzheimerDisease.RData_/AlzheimerDisease.Rdata")
head(predictors, n = 1)


library(ISLR)
head(Smarket)
pairs(Smarket)

glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs <- predict(glm.fit, type = 'response')
glm.probs[1:10]

library(tidyverse)
library(MASS)
# lab 1
mpg <- read.csv("data/Auto.csv")

head(mpg)
colSums(is.na(mpg)) # no NAs
med_mpg <- median(mpg$mpg)
mpg <- mpg %>%
  mutate(mpg01 = ifelse(mpg > med_mpg, 1, 0))
train = (mpg$year %% 2 == 0)
test = (mpg$year %% 2 != 0)
trainXy = mpg[train,]
testXy = mpg[test,]
# mpg$cylinders = as.numeric(mpg$cylinders)
# train$cylinders = as.factor(train$cylinders)
# train$cylinders = as.factor(train$horsepower)
# train$mpg01 = as.factor(train$mpg01)
lda.fit <- lda(mpg01~cylinders+weight+displacement+horsepower, data = mpg, subset = train) # ???
lda.class <- predict(lda.fit, mpg[test,])$class
table(lda.class, mpg[test,]$mpg01)

qda.fit <-  qda(mpg01~weight+displacement+horsepower, data = mpg) # ???
qda.class <- predict(qda.fit, mpg[test,])$class
table(qda.class, mpg[test,]$mpg01)

logistic.fit <- glm(mpg01~cylinders+weight+displacement+horsepower, data = mpg, family = binomial, subset = train)
logistic.pred <- predict(logistic.fit, mpg[test,], type="response")
pred <- ifelse(logistic.pred>0.5, 1, 0)
table(mpg$mpg01, pred)

trainX = trainXy %>%
  dplyr::select(weight,displacement,horsepower)
trainy = trainXy %>%
  dplyr::select(mpg01)
trainy = as.factor(trainy$mpg01)
testX = testXy %>%
  dplyr::select(weight,displacement,horsepower)
testy = testXy %>%
  dplyr::select(mpg01)
testy = as.factor(testy$mpg01)

library(class)
trainX$horsepower <- as.numeric(trainX$horsepower)
knn.pred = knn(trainX, testX, trainy, k = 3) # doesn't work... why?


# Lab 2
Default <- read.table("data/data/data/default.txt")
head(Default)
def_glm.fit <- glm(default~income+balance, data = Default, family = binomial)
summary(def_glm.fit)

boot.fn <- function(dat, idx){
  my_glm <- glm(default~income+balance, data = dat, family = binomial, subset = idx)
  coeff <- coef(my_glm)
  return(coeff)
}
library(boot)
boot(Default, boot.fn, 50)