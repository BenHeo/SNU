library(datasets)
library(MASS)
library(ISLR)

advertising = read.csv('data/Advertising.csv', row.names = NULL)
lm.fit <- lm(sales ~ TV, data = advertising)
summary(lm.fit)

head(Credit)
lm.fit <- lm(Balance~Gender, Credit)
summary(lm.fit)

attach(Credit)
plot(Balance~Income, col = Gender)
lm.fit <- lm(Balance~Income+Gender)
lm.fit$coefficients
mylm <- lm(Balance~., data = Credit)
summary(mylm)
aic.credit <- stepAIC(mylm, direction = "both")
summary(aic.credit)
