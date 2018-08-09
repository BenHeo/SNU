library(ggplot2)
ing = read.csv("data/insurance/insurance.csv", header = T)
ing = ing[,-c(3,4)]
ing$clm = as.factor(ing$clm)
head(ing)
ggplot(data = ing, aes(exposure, veh_value)) + geom_point(alpha = 0.1, aes(color = clm))
load("data/AlzheimerDisease.RData_/AlzheimerDisease.Rdata")
head(predictors, n = 1)
