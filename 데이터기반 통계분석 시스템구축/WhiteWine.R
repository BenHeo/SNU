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
# draw histograms
hist(fixed_acidity, breaks = 20) # normal with outliers
hist(volatile_acidity, breaks = 20) # right skew
hist(citric_acid, breaks = 30) # # outliers 
hist(residual_sugar, breaks = 10) # right skew
hist(chlorides, breaks = 20) # some ranges have large number of observations ==> better to log
hist(log(chlorides), breaks = 20)
hist(free_sulfur_dioxide, breaks = 30) # almost normal
hist(total_sulfur_dioxide, breaks = 20) # normal
hist(density, breaks = 20) # normal but outliers
hist(pH, breaks = 20) # normal
hist(sulphates, breaks = 20) # normal
hist(alcohol, breaks = 30) # almost normal
hist(quality, breaks = 20) # discrete data, unbalance exists between data
table(quality) # 20  163  1457  2198  880  175  5


# draw boxplots about quality
boxplot(fixed_acidity~quality, col = 3:9)
boxplot(volatile_acidity~quality, col = 3:9)
boxplot(citric_acid~quality, col = 3:9)
boxplot(residual_sugar~quality, col = 3:9)
boxplot(chlorides~quality, col = 3:9)
boxplot(log(chlorides)~quality, col = 3:9)
boxplot(free_sulfur_dioxide~quality, col = 3:9)
boxplot(total_sulfur_dioxide~quality, col = 3:9)
boxplot(density~quality, col = 3:9)
boxplot(pH~quality, col = 3:9)
boxplot(sulphates~quality, col = 3:9)
boxplot(alcohol~quality, col = 3:9)
