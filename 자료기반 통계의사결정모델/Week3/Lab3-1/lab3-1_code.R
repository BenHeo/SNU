#####################################################################################
#                           Many statistical analyses                               #
#                                   2018.08.22                                      #
#                   Instructor : Sungkyu Jung, TA : Boyoung Kim                     #
#                                                                                   #
#####################################################################################



#--- Australian twin sample biometric data

library(tidyr)
library(dplyr)

#install.packages("OpenMx")
library(OpenMx)
data(twinData)
twinData <- as_tibble(twinData)
table(twinData$zygosity)

help(twinData)


#scatter plot
library(ggplot2)
twinData %>% ggplot(mapping = aes(ht1, ht2)) + 
  geom_point() 

#check correlation
cor(twinData$ht1, twinData$ht2, use="complete.obs")

#test whether two variables are correlated
twinData %>% 
  with(cor.test(~ ht1 + ht2, alternative = "greater"))

#test for each subgroup
twinData %>% 
  group_by(cohort,zygosity) %>% 
  summarize(cor.test(~ ht1 + ht2, data = .))
# output of cor.test is list ==> We need to change it by broom


#--- Combine results from multiple analyses using broom

library(broom)
cor_result <- cor.test(~ ht1 + ht2, data = twinData)
tidy_cor_result <- tidy(cor_result)

str(cor_result) #list

str(tidy_cor_result) #data frame

# summarize() must be of the form "var=value"
twinData %>% 
  group_by(cohort,zygosity) %>%  
  summarize(tidy( cor.test(~ ht1 + ht2, alternative = "greater" , data = . )))

# do() returns either a data frame or arbitrary objects
twinData %>% 
  group_by(cohort,zygosity) %>%  
  do(tidy( cor.test(~ ht1 + ht2, alternative = "greater" , data = . )))




#######################
h_twinData <- twinData %>% 
  group_by(cohort,zygosity) %>%  
  do(tidy( cor.test(~ ht1 + ht2, alternative = "greater" , data = . )))

twinData %>%
  inner_join(h_twinData, by = c("cohort", "zygosity")) %>%
  ggplot(aes(ht1, ht2, color = conf.low)) + geom_point() + facet_grid(cohort~zygosity)

w_twinData <- twinData %>% 
  group_by(cohort,zygosity) %>%  
  do(tidy( cor.test(~ wt1 + wt2, alternative = "greater" , data = . )))

twinData %>%
  inner_join(w_twinData, by = c("cohort", "zygosity")) %>%
  ggplot(aes(wt1, wt2, color = conf.low)) + geom_point() + facet_grid(cohort~zygosity)

ggplot(twinData, aes(order, height)) + facet_grid(cohort~zygosity)
