library(tidyverse)
library(data.table)
library(randomForest)
set.seed(1)
mail <- fread('mail_order.csv')
mail
ggplot(mail, aes(x=purchase)) + geom_bar() # unbalanced
# train_idx <- sample.int(4000, 2000, replace = FALSE)
training_idx <- 1:2000
training <- mail[training_idx,]
testing <- mail[-training_idx,]

# 1
refer_rand_idx <- sample.int(2000, 500, replace = FALSE)
refer_df <- testing[refer_rand_idx,]
refer_prop <- sum(as.integer(refer_df$purchase))/500
refer_prop # reference prop is 0.064 or 6.4%

# 2
nMail <- mail %>% 
  mutate(R = ifelse(recency <= 12, 2, 1),
         `F` = ifelse(frequency >=3, 2, 1),
         M = ifelse(monetary >= 209, 2, 1))

nMail[-training_idx,] %>% 
  group_by(R,`F`,M) %>%
  summarise(n = n(),
            proba = sum(purchase)/n) %>%
  arrange(desc(proba)) # 221 => 222 => 212 => 211 => 122 => 121 => 112 => 111

nMail[-training_idx,] %>%
  group_by(R, `F`, M) %>%
  summarise(n = n(),
            proba = sum(purchase)/n) %>%
  arrange(desc(proba))

nMailTest <- nMail[-training_idx,] %>%
  filter(R == 2,
         `F` == 2,
         M %in% c(1, 2))

nMailPlus <- nMail[-training_idx,] %>%
  filter(R == 2,
         `F` == 1,
         M == 2) %>%
  sample_n(100)

nMailTest <- nMailTest %>%
  bind_rows(nMailPlus)

sum(as.integer(nMailTest$purchase))/500 # 0.158


# 3
fMail <- mail %>%
  mutate(R = ifelse(recency > 16, 1, ifelse(recency > 12, 2, 
                                            ifelse(recency > 8, 3, ifelse(recency > 4, 4, 5)))),
         `F` = ifelse(frequency == 1, 1, ifelse(frequency == 2, 2, 
                                               ifelse(frequency <= 5, 3, 
                                                      ifelse(frequency <= 9, 4, 5)))),
         M = ifelse(monetary <= 113, 1, ifelse(monetary <= 181, 2, 
                                            ifelse(monetary <= 242, 3, 
                                                   ifelse(monetary <= 299, 4, 5))))
  )
k <- fMail[training_idx,] %>%
  group_by(R, `F`, M) %>%
  summarise(n = n(),
    proba = sum(purchase)/n) %>%
  arrange(desc(proba)) # 454 => 415 => 442 => 452 => 552 => 532 => 434 => 234 => 343 => 545 => ...


tt <- fMail[-training_idx,] %>%
  group_by(R, `F`, M) %>%
  summarise(test_n = n(),
            test_purchase = sum(purchase),
            test_proba = sum(purchase)/test_n) %>%
  arrange(desc(test_proba))

joined_RFM <- k %>% 
  inner_join(tt)
x = 0
cnt = 0
while(cnt < 500){
  x = x+1
  cnt = cnt + joined_RFM[x,"test_n"]
}
x
joined_RFM[x+1,]

f_joined_RFM <- joined_RFM[1:37,]
sum(f_joined_RFM$test_n)
joined_RFM[38,]
f_RFM_plus <- fMail[-training_idx,] %>%
  filter(R == 3, `F` == 4, M == 4) %>%
  sample_n(8) %>%
  group_by(R, `F`, M) %>%
  summarise(test_n = n(),
            test_purchase = sum(purchase),
            test_proba = sum(purchase)/test_n)

ff <- f_joined_RFM %>%
  select(-c(n, proba)) %>%
  bind_rows(f_RFM_plus)

ff[,4:5] %>%
  summarise(n = sum(test_n),
          purchase = sum(test_purchase),
          proba = purchase/n) # number of customers = 500, number of purchase = 62 => 12.4

# 4
reg_train <- mail[training_idx,]
reg_test <- mail[-training_idx,]
lm.fit <- lm(purchase~monetary+recency+frequency, reg_train)

pred_lm <- predict(lm.fit, reg_test)
pp <- tibble(pred = pred_lm, purchase = reg_test$purchase)
pp %>%
  arrange(desc(pred)) %>%
  head(500) %>%
  summarise(t_purchs = sum(purchase)/500) # 0.16

# 5
mail$purchase <- as.factor(mail$purchase)
mail$gender <- as.factor(mail$gender)
train <- mail[training_idx,]
test <- mail[-training_idx,]
ggplot(train, aes(x=purchase)) + geom_bar()
ggplot(test, aes(x=purchase)) + geom_bar()

ggplot(mail, aes(x=recency, fill=purchase)) + geom_bar()
ggplot(mail, aes(x=frequency, fill=purchase)) + geom_bar()
ggplot(mail, aes(x=monetary, fill=purchase)) + geom_bar()
ggplot(mail, aes(x=duration, fill=purchase)) + geom_bar()
ggplot(mail, aes(x=gender, fill=purchase)) + geom_bar()

randomForest(purchase ~ gender+monetary+recency+frequency+duration, data = train,
             xtest = test[,-c(1,7)], ytest = test$purchase, ntree = 1000, 
             maxnodes = 7, importance = TRUE)

train1 <- train %>%
  filter(purchase == 1)

Btrain <- train1
for (i in 1:10){
  train1B <- train1
  rand.mat <- matrix(rnorm(163*4, 0, 2), 163, 4)
  random_train <- as.matrix(train1B[,-c(1,2,7)]) + rand.mat
  train1B[, -c(1,2,7)] <- random_train
  Btrain <- bind_rows(Btrain, train1B)
}
Ntrain <- bind_rows(train, Btrain)
ggplot(Ntrain, aes(x=purchase)) + geom_bar()

glm.fit <- glm(purchase ~ gender+monetary+recency+frequency+duration, data = Ntrain, 
               family = "binomial")
step(glm.fit, direction = "both")

glm.fit <- glm(purchase ~ gender+monetary+recency+frequency+duration, data = Ntrain, 
               family = "binomial")
glm.prob <- predict(glm.fit, test)
glm.pred <- ifelse(glm.prob>0.5, 1, 0)
glm.conf_mat <- table(purchase = test$purchase, glm.pred); glm.conf_mat
summary(glm.fit)
glm.conf_mat[2,2]/(sum(glm.conf_mat[2,]))
(glm.conf_mat[1,1]+glm.conf_mat[2,2])/sum(glm.conf_mat)


randomForest(purchase ~ gender+monetary+recency+frequency+duration, data = Ntrain,
             xtest = test[,-c(1,7)], ytest = test$purchase, ntree = 1000, 
             maxnodes = 5, importance = TRUE)



##################################################
# Bad...
library(gbm)
mail.boost=gbm(purchase ~ gender+monetary+recency+frequency+duration, data = train,
                 distribution = "bernoulli",n.trees = 500, n.minobsinnode = 50,
                 shrinkage = 0.01, interaction.depth = 4, cv.folds = 4)
summary(mail.boost)
plot(mail.boost, i="monetary")
plot(mail.boost, i="frequency")
n.trees = seq(from=100 ,to=10000, by=100)

bestTreeForPrediction = gbm.perf(mail.boost)
gbm_pred <- predict(mail.boost, test, n.trees = n.trees); gbm_pred
sort(gbm_in_pred, decreasing = T)
sort(gbm_pred, decreasing = T)
test[order(gbm_pred, decreasing = T),] %>% head(300) %>%
  summarise(ss = sum(purchase)/300)




predmatrix<-predict(mail.boost, test, n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(test, apply( (predmatrix-purchase)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

