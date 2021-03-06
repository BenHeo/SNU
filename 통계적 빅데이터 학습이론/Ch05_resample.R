library(tidyverse)
library(knitr)
library(ISLR)
opts_chunk$set(eval=TRUE, cache=TRUE, fig.width=7, fig.height=4)
auto = read.csv("data/Auto.csv", header = T)
head(auto)
auto$horsepower <- as.numeric(auto$horsepower)

trainIdx = sample(397, 198)
lm.fit <- lm(mpg~horsepower, data = auto, subset=train)
lm.pred <- predict(lm.fit, auto[-train,])
mean((mpg[-train,]$mpg - lm.pred)^2)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data = auto, )