library(tidyverse)
library(xlsx)
fin4 <- read.xlsx("data4.xlsx", 2, startRow = 2)
head(fin4)
fin4 <- fin4 %>%
  select(-NA.)

apply(fin4[,2:51], 1, mean)
apply(fin4[,2:51], 1, sd)
apply(fin4[,2:51], 2, mean)
apply(fin4[,2:51], 2, sd)

set.seed(1)
k_clust <- kmeans(fin4[,2:51], 2, nstart = 20); k_clust
k_clust$cluster
forplot <- bind_cols(fin4[,1:4], cluster = k_clust$cluster)
forplot$V1 <- as.factor(forplot$V1)
ggplot(forplot) + geom_point(aes(V3, V4, color = cluster, shape = V1), size = 2)


###################### 2 ########################
train_idx <- sample.int(500, 400)
train_fin4 <- fin4[train_idx, ]
test_fin4 <- fin4[-train_idx, ]
glm.fit <- glm(V1~., data = train_fin4, family = "binomial")
step(glm.fit, direction = "both")
glm.fit <- glm(V1~ V11 + V16 + V17 + V25 + V35 + V37, data = train_fin4, family = "binomial")
glm.pred <- predict(glm.fit, test_fin4, type = "response")
trans_pred <- ifelse(glm.pred<0.5, 0, 1)
table(test_fin4$V1, trans_pred)
sum(test_fin4$V1==trans_pred)/nrow(test_fin4)

random01 <- sample(0:1, 500, replace = TRUE)
table(fin4$V1, random01)
sum(fin4$V1==random01)/nrow(fin4)


####################### 10 fold ############################

n_fin4 <- fin4[sample(1:500),]

ten_tables2 <- matrix(0, 2, 2)
colnames(ten_tables2) <- paste0("pred", 0:1)
rownames(ten_tables2) <- paste0("real", 0:1)
ten_tables2 <- as.table(ten_tables2)
for (i in 1:5){
  idx <- ((i-1)*100+1):(i*100)
  test_set <- n_fin4[idx,]
  random01 <- sample(0:1, 100, replace = TRUE)
  itable <- table(test_set$V1, random01)
  ten_tables2 <- ten_tables2 + as.table(itable)
}
ten_tables2
corrected <- sum(diag(ten_tables2))
corrected/sum(ten_tables2)
