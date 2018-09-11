library(tidyverse)
library(xlsx)
poli_rel <- read.xlsx("data5.xlsx", 1)
poli_party <- read.xlsx2("data5.xlsx", 2)

diag(poli_rel)
legi_order <- poli_rel$NA.
rownames(poli_rel) <- legi_order
poli_rel <- poli_rel %>%
  select(-1)
diag(as.matrix(poli_rel))
poli_rel[which.max(diag(as.matrix(poli_rel))),]


group_poli <- rep(0, 141)
influence_order <- order(diag(as.matrix(poli_rel)), decreasing = FALSE)
j <- 0
for (i in influence_order){
  if(group_poli[i] == 0){
    j <- j+1
    k <- j
  } else{
    k <- group_poli[i]
  }
  ith_poli<- poli_rel[i,]
  refer <- poli_rel[i,i]
  related <- which(ith_poli >= (refer/5))
  group_poli[related] <- k
}

ggplot(data.frame(x=group_poli)) + geom_bar(aes(as.factor(x)))


poli_party_clust <- bind_cols(poli_party, clust=group_poli)
View(poli_party_clust)
