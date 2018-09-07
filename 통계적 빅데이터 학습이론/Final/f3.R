library(tidyverse)
library(data.table)
library(xlsx)
political <- read.xlsx("data3.xlsx", 1)
political$k3[political$k3==2] <- 0   # change 1,2 to 0,1

################ EDA ######################
head(political)
summary(political)
colSums(is.na(political))
colSums(political[9:18], na.rm = T)
nrow(political)

ggplot(political) + geom_bar(aes(ideo_self)) # total dist

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
ggplot(political[k7is1,]) + geom_bar(aes(ideo_self)) # conservative
ggplot(political[-k7is1,]) + geom_bar(aes(ideo_self)) # neutral

k8is1 <- which(political[,"k8"] == 1)
ggplot(political[k8is1,]) + geom_bar(aes(ideo_self)) # very conservative
ggplot(political[-k8is1,]) + geom_bar(aes(ideo_self)) # neutral

k10is1 <- which(political[,"k10"] == 1)
ggplot(political[k10is1,]) + geom_bar(aes(ideo_self)) # progress
ggplot(political[-k10is1,]) + geom_bar(aes(ideo_self)) # little conservative

k12is1 <- which(political[,"k12"] == 1)
ggplot(political[k12is1,]) + geom_bar(aes(ideo_self)) # progress
ggplot(political[-k12is1,]) + geom_bar(aes(ideo_self)) # conservative

k13is1 <- which(political[,"k13"] == 1)
ggplot(political[k13is1,]) + geom_bar(aes(ideo_self)) # very very conservative
ggplot(political[-k13is1,]) + geom_bar(aes(ideo_self)) # progress

k14is1 <- which(political[,"k14"] == 1)
ggplot(political[k14is1,]) + geom_bar(aes(ideo_self)) # little conservative
ggplot(political[-k14is1,]) + geom_bar(aes(ideo_self)) # little progress

political %>%
  mutate(pred = ifelse(is.na(k2), 0, ifelse(k2==1, 1, -1)) +
           ifelse(is.na(k3), 0, ifelse(k3==1, 1, -1)) +
           ifelse(is.na(k4), 0, ifelse(k4==1, 2, -1)) +
           ifelse(is.na(k6), 0, ifelse(k6==1, 1, -1)) +
           ifelse(is.na(k7), 0, ifelse(k7==1, 2, 0)) +
           ifelse(is.na(k8), 0, ifelse(k8==1, 2, 0)) +
           ifelse(is.na(k10), 0, ifelse(k10==1, -1, 0)) +
           ifelse(is.na(k12), 0, ifelse(k12==1, -1, 0)) +
           ifelse(is.na(k13), 0, ifelse(k13==1, 3, -1)) +
           ifelse(is.na(k14), 0, ifelse(k14==1, 0, 0)) +
           5
         ) %>%
  mutate(standardized_pred = ifelse(pred==-2, 0, 
                                    ifelse(pred%in%c(-1,0), 1,
                                           ifelse(pred%in%c(1,2), 2,
                                                  ifelse(pred%in%c(3,4), 3, 
                                                         ifelse(pred%in%c(5,6), 4,
                                                                ifelse(pred%in%c(7,8), 5,
                                                                       ifelse(pred%in%c(9,10), 6,
                                                                              ifelse(pred%in%c(11,12), 7,
                                                                                     ifelse(pred%in%c(13,14), 8, 
                                                                                            ifelse(pred%in%c(15,16), 9, 10
                                                                                                   ))))))))))) %>%
  ggplot(aes(standardized_pred)) + geom_bar()
