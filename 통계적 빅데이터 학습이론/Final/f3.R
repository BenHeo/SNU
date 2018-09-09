library(tidyverse)
library(data.table)
library(xlsx)
political <- read.xlsx("data3.xlsx", 1)
political$k3[political$k3==2] <- 0   # change 1,2 to 0,1
sum(political$ideo_self==5)/nrow(political) # at least this...
################ EDA ######################
head(political)
summary(political)
colSums(is.na(political))
colSums(political[9:18], na.rm = T)
nrow(political)

ggplot(political) + geom_bar(aes(ideo_self)) # total dist

ggplot(political) + geom_bar(aes(sex))
male <- which(political[,"sex"] == 1)
ggplot(political[male,]) + geom_bar(aes(ideo_self)) # conservative
ggplot(political[-male,]) + geom_bar(aes(ideo_self)) # neutral

ggplot(political) + geom_boxplot(aes(age, ideo_self, group = age))

yg <- which(political[,"age1"] <= 50)
ggplot(political[yg,]) + geom_bar(aes(ideo_self)) # neutral
ggplot(political[-yg,]) + geom_bar(aes(ideo_self)) # very conservative

ggplot(political) + geom_boxplot(aes(edu, ideo_self, group = edu)) # if 1 little progress, if 5 little conservative, else neutral

ggplot(political) + geom_boxplot(aes(area, ideo_self, group = area)) + 
  geom_text(aes(area, 2, label=area))
# conservative : 2, 7, 10, 11, 14
# progress : 5, 9, 12, 15, 16

ggplot(political) + geom_boxplot(aes(income, ideo_self, group = income)) + 
  geom_text(aes(income, 2, label=income))
# conservative : 8, 14, 15
# progress : 2, 3, 5, 6, 7, 9

k2is1 <- which(political[,"k2"] == 1)
ggplot(political[k2is1,]) + geom_bar(aes(ideo_self)) # conservative
ggplot(political[-k2is1,]) + geom_bar(aes(ideo_self)) # progress

k3is1 <- which(political[,"k3"] == 1)
ggplot(political[k3is1,]) + geom_bar(aes(ideo_self)) # conservative
ggplot(political[-k3is1,]) + geom_bar(aes(ideo_self)) # progress

k4is1 <- which(political[,"k4"] == 1)
ggplot(political[k4is1,]) + geom_bar(aes(ideo_self)) # very conservative
ggplot(political[-k4is1,]) + geom_bar(aes(ideo_self)) # progress

k6is1 <- which(political[,"k6"] == 1)
ggplot(political[k6is1,]) + geom_bar(aes(ideo_self)) # conservative
ggplot(political[-k6is1,]) + geom_bar(aes(ideo_self)) # progress

k7is1 <- which(political[,"k7"] == 1)
ggplot(political[k7is1,]) + geom_bar(aes(ideo_self)) # very very conservative
ggplot(political[-k7is1,]) + geom_bar(aes(ideo_self)) # neutral

k8is1 <- which(political[,"k8"] == 1)
ggplot(political[k8is1,]) + geom_bar(aes(ideo_self)) # very very conservative
ggplot(political[-k8is1,]) + geom_bar(aes(ideo_self)) # neutral

k10is1 <- which(political[,"k10"] == 1)
ggplot(political[k10is1,]) + geom_bar(aes(ideo_self)) # progress
ggplot(political[-k10is1,]) + geom_bar(aes(ideo_self)) # little conservative

k12is1 <- which(political[,"k12"] == 1)
ggplot(political[k12is1,]) + geom_bar(aes(ideo_self)) # progress
ggplot(political[-k12is1,]) + geom_bar(aes(ideo_self)) # conservative

k13is1 <- which(political[,"k13"] == 1)
ggplot(political[k13is1,]) + geom_bar(aes(ideo_self)) # very very very conservative
ggplot(political[-k13is1,]) + geom_bar(aes(ideo_self)) # progress

k14is1 <- which(political[,"k14"] == 1)
ggplot(political[k14is1,]) + geom_bar(aes(ideo_self)) # little conservative
ggplot(political[-k14is1,]) + geom_bar(aes(ideo_self)) # little progress

std_poli <- political %>%
  mutate(pred = ifelse(is.na(k2), 0, ifelse(k2==1, 1, -1)) +
           ifelse(is.na(k3), 0, ifelse(k3==1, 1, -1)) +
           ifelse(is.na(k4), 0, ifelse(k4==1, 1.5, -1)) +
           ifelse(is.na(k6), 0, ifelse(k6==1, 1, -1)) +
           ifelse(is.na(k7), 0, ifelse(k7==1, 2, 0)) +
           ifelse(is.na(k8), 0, ifelse(k8==1, 2, 0)) +
           ifelse(is.na(k10), 0, ifelse(k10==1, -1, 0.5)) +
           ifelse(is.na(k12), 0, ifelse(k12==1, -1, 1)) +
           ifelse(is.na(k13), 0, ifelse(k13==1, 2.5, -1)) +
           ifelse(is.na(k14), 0, ifelse(k14==1, 0.5, -0.5)) +
           ifelse(sex==1, 1, 0) +
           ifelse(age1>50, 1.5, 0) +
           ifelse(edu==1, -0.5, ifelse(edu==5, 0.5, 0)) +
           ifelse(area %in% c(2, 7, 10, 11, 14), 1, ifelse(area %in% c(5, 9, 12, 15, 16), -1, 0)) +
           ifelse(income %in% c(8, 14, 15), 1, ifelse(income %in% c(2, 3, 5, 6, 7, 9), -1, 0))
         ) %>%
  mutate(standard_pred = ifelse(pred < -7, 0,
                                ifelse(pred >= -7 & pred < -5, 1,
                                       ifelse(pred >= -5 & pred < -3, 2,
                                                  ifelse(pred >= -3 & pred < -1.5, 3, 
                                                         ifelse(pred >= -1.5 & pred < 0, 4,
                                                                ifelse(pred >= 0 & pred < 4, 5,
                                                                       ifelse(pred >= 4 & pred < 6.5, 6,
                                                                              ifelse(pred >= 6.5 & pred < 9, 7,
                                                                                     ifelse(pred >= 9 & pred < 12, 8, 
                                                                                            ifelse(pred >= 12 & pred < 15, 9, 10
                                                                                                   ))))))))))) #%>%
  # ggplot(aes(standard_pred)) + geom_bar()


table(real = std_poli$ideo_self,pred = std_poli$standard_pred)
sum(std_poli$ideo_self == std_poli$standard_pred)/nrow(std_poli)

ggplot(std_poli,aes(standard_pred)) + geom_bar() 

################################# end of rule based prediction ########################
################################# use statistical learning ###########################
polyforest <- political %>%
  select(k2:k14)
polyforest[polyforest==0] <- -1 # change 0 to 1
polyforest[is.na(polyforest)] <- 0 # change NA to 0
political2 <- political
political2[,9:18] <- polyforest
pol2 <- political2 %>% 
  select(sex, age1, area:ideo_self)
pol2$ideo_self <- as.factor(pol2$ideo_self)
pol2$area <- as.factor(pol2$area)
train <- sample.int(1054, 800)
pol2_train <- pol2[train,]
pol2_test <- pol2[-train,]
library(randomForest)
randomForest(ideo_self ~ ., data = pol2_train,
             xtest = pol2_test[,-15], ytest = pol2_test$ideo_self, ntree = 1000, 
             maxnodes = 5, importance = TRUE)


library(gbm)
set.seed(1)
boost.boston=gbm(ideo_self~.,data=pol2_train,
                 distribution="multinomial",n.trees=1000, interaction.depth = 1, cv.folds = 5)
summary(boost.boston)
best.iter = gbm.perf(boost.boston, method="cv")
predict(boost.boston, newdata = pol2_test, n.trees = 1000)


library(caret)
library(e1071)
fitControl = trainControl(method="cv", number=5, returnResamp = "all")
model2 = train(ideo_self~.,data=pol2_train, method="gbm",distribution="multinomial",
               trControl=fitControl, verbose=F,
               tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
model2
# confusionMatrix(model2)
mPred = predict(model2, pol2_test, na.action = na.pass)
postResample(mPred, pol2_test$ideo_self)
sum(mPred == pol2_test$ideo_self)/nrow(pol2_test)
table(pred = mPred, real = pol2_test$ideo_self)





################### 10-folds ########################
polyforest <- political %>%
  select(k2:k14)
polyforest[polyforest==0] <- -1 # change 0 to 1
polyforest[is.na(polyforest)] <- 0 # change NA to 0
political2 <- political
political2[,9:18] <- polyforest
pol2 <- political2 %>% 
  select(sex, age1, area:ideo_self)
pol2$ideo_self <- as.factor(pol2$ideo_self)
pol2$area <- as.factor(pol2$area)
library(gbm)
set.seed(1)
library(caret)
library(e1071)
fold_point <- c(seq(0, 900, 100), 1054)
ten_tables <- matrix(0, 11, 11)
colnames(ten_tables) <- paste0("pred", 0:10)
rownames(ten_tables) <- paste0("real", 0:10)
ten_tables <- as.table(ten_tables)
for (i in 1:10){
  start_point <- fold_point[i]+1
  end_point <- fold_point[i+1]
  test_idx <- start_point:end_point
  test_set <- pol2[test_idx,]
  train_set <- pol2[-test_idx,]
  boost.pol=gbm(ideo_self~.,data=train_set,
                   distribution="multinomial",n.trees=500, interaction.depth = 1, cv.folds = 5)
  summary(boost.pol)
  best.iter = gbm.perf(boost.pol, method="cv")
  # predict(boost.pol, newdata = test_set, n.trees = 1000)
  fitControl = trainControl(method="cv", number=5, returnResamp = "all")
  model2 = train(ideo_self~.,data=train_set, method="gbm",distribution="multinomial",
                 trControl=fitControl, verbose=F,
                 tuneGrid=data.frame(.n.trees=best.iter, .shrinkage=0.01, .interaction.depth=1, .n.minobsinnode=1))
  mPred = predict(model2, test_set, na.action = na.pass)
  postResample(mPred, test_set$ideo_self)
  sum(mPred == test_set$ideo_self)/nrow(test_set)
  itable <- table(real = test_set$ideo_self, pred = mPred)
  ten_tables <- ten_tables + as.table(itable)
  cat(i, "fold completed")
}
ten_tables
corrected <- sum(diag(ten_tables))
corrected/sum(ten_tables)



for (i in 1:10){
  start_point <- fold_point[i]+1
  end_point <- fold_point[i+1]
  test_idx <- start_point:end_point
  test_set <- pol2[test_idx,]
  train_set <- pol2[-test_idx,]
  ntree <- 50
  boost.pol=gbm(ideo_self~.,data=train_set,
                distribution="multinomial",n.trees=ntree, 
                shrinkage = 0.1, interaction.depth = 1)
  
  fitControl = trainControl(method="cv", number=5, returnResamp = "all")
  model2 = train(ideo_self~.,data=train_set, method="gbm",distribution="multinomial",
                 trControl=fitControl, verbose=F,
                 tuneGrid=data.frame(.n.trees=ntree, .shrinkage=0.1, .interaction.depth=1, .n.minobsinnode=1))
  mPred = predict(model2, test_set, na.action = na.pass)
  itable <- table(real = test_set$ideo_self, pred = mPred)
  ten_tables <- ten_tables + as.table(itable)
}
