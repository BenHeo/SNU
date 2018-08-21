library(tidyverse)
library(data.table)
library(randomForest)
mail <- fread('mail_order.csv')
mail
mail$purchase <- as.factor(mail$purchase)
mail$gender <- as.factor(mail$gender)
ggplot(mail, aes(x=purchase)) + geom_bar() # unbalanced
train_idx <- sample.int(4000, 2000, replace = FALSE)
train <- mail[train_idx,]
test <- mail[!train_idx,]
ggplot(train, aes(x=purchase)) + geom_bar()
ggplot(test, aes(x=purchase)) + geom_bar()

randomForest(purchase ~ gender+monetary+recency+frequency+duration, data = train,
             xtest = test[,-c(1,7)], ytest = test$purchase, ntree = 1000, 
             maxnodes = 7, importance = TRUE)

train1 <- train %>%
  filter(purchase == 1)

Btrain <- train1
for (i in 1:10){
  train1B <- train1
  rand.mat <- matrix(rnorm(154*4), 154, 4)
  random_train <- as.matrix(train1B[,-c(1,2,7)]) + rand.mat
  train1B[, -c(1,2,7)] <- random_train
  Btrain <- bind_rows(Btrain, train1B)
}
Ntrain <- bind_rows(train, Btrain)
ggplot(Ntrain, aes(x=purchase)) + geom_bar()
randomForest(purchase ~ gender+monetary+recency+frequency+duration, data = Ntrain,
             xtest = test[,-c(1,7)], ytest = test$purchase, ntree = 1000, 
             maxnodes = 5, importance = TRUE)
