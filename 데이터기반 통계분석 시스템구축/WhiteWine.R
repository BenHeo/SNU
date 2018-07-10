# Reference :
# https://www.sciencedirect.com/science/article/pii/S0167923609001377?via%3Dihub

library(tidyverse)
white <- read.csv('data/winequality-white.csv', sep = ';')
names(white) <- c("fixed_acidity", "volatile_acidity", "citric_acid", "residual_sugar", "chlorides", "free_sulfur_dioxide",
                  "total_sulfur_dioxide", "density", "pH", "sulphates", "alcohol", "quality" )

head(white)
summary(white)
str(white)
attach(white)
boxplot(fixed_acidity~quality, col = 3:9)
boxplot(volatile_acidity~quality, col = 3:9)
boxplot(citric_acid~quality, col = 3:9)
boxplot(residual_sugar~quality, col = 3:9)
boxplot(chlorides~quality, col = 3:9)
boxplot(free_sulfur_dioxide~quality, col = 3:9)
boxplot(total_sulfur_dioxide~quality, col = 3:9)
boxplot(density~quality, col = 3:9)
boxplot(pH~quality, col = 3:9)
boxplot(sulphates~quality, col = 3:9)
boxplot(alcohol~quality, col = 3:9)
