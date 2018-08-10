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
mpg <- read.csv("data/data/data/mpg.csv")

head(mpg)
colSums(is.na(mpg)) # no NAs
med_mpg <- median(mpg$mpg)
mpg <- mpg %>%
  mutate(mpg01 = ifelse(mpg > med_mpg, 1, 0))
train = (mpg$year %% 2 == 0)
test = (mpg$year %% 2 != 0)
train = mpg[train,]
test = mpg[test,]
mpg$cylinders = as.numeric(mpg$cylinders)
train$cylinders = as.numeric(train$cylinders)
lda.fit <- lda(mpg01~cylinders+weight+displacement+horsepower, data = mpg, subset = train) # ???
lda.class <- predict(lda.fit, test)$class
table(lda.class, test$mpg01)


qda.fit <-  qda(mpg01~cylinders+weight+displacement+horsepower, data = mpg, subset = train)
