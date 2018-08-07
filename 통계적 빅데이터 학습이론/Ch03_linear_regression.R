library(datasets)
library(MASS)
library(ISLR)

advertising = read.csv('data/Advertising.csv')
lm.fit <- lm(sales ~ TV, data = advertising)
summary(lm.fit)

head(Credit)
lm.fit <- lm(Balance~Gender, Credit)
summary(lm.fit)
 