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
boxplot(fixed_acidity)
hist(volatile_acidity, breaks = 20) # right skew
hist(log(volatile_acidity), breaks = 20) # normal
boxplot(log(volatile_acidity))
hist(citric_acid, breaks = 30) # outliers 
hist(sqrt(citric_acid), breaks = 30) # almost normal
hist(residual_sugar, breaks = 50) # right skew
hist(log(residual_sugar), breaks = 50) # two modes
hist(chlorides, breaks = 20) # some ranges have large number of observations ==> better to log
hist(log(chlorides), breaks = 20)
hist(free_sulfur_dioxide, breaks = 30) # almost normal
hist(sqrt(free_sulfur_dioxide), breaks = 30) # almost normal
hist(total_sulfur_dioxide, breaks = 20) # normal
hist(density, breaks = 50) # normal but 3 outliers
hist(pH, breaks = 20) # normal
hist(sulphates, breaks = 20) # normal
hist(log(sulphates), breaks = 20) # normal
hist(alcohol, breaks = 50) # three main curves
hist(quality, breaks = 20) # discrete data, unbalance exists between data
table(quality) # 20  163  1457  2198  880  175  5

# 3-5 6 7-9
# low mid high

# draw boxplots about quality
boxplot(fixed_acidity~quality, col = 3:9)
boxplot(log(volatile_acidity)~quality, col = 3:9)
boxplot(citric_acid~quality, col = 3:9)
boxplot(sqrt(citric_acid)~quality, col = 3:9)
boxplot(residual_sugar~quality, col = 3:9)
boxplot(chlorides~quality, col = 3:9)
boxplot(log(chlorides)~quality, col = 3:9)
boxplot(sqrt(free_sulfur_dioxide)~quality, col = 3:9)
boxplot(total_sulfur_dioxide~quality, col = 3:9)
boxplot(density~quality, col = 3:9)
boxplot(pH~quality, col = 3:9)
boxplot(sulphates~quality, col = 3:9)
boxplot(log(sulphates)~quality, col = 3:9)
boxplot(alcohol~quality, col = 3:9)

# 단맛, 신맛, 짠맛, 산화제로 나눌 수 있음
# 산화제는 높으면 오래 보존되지만, 몸에 해롭다
# 짠맛은 영향력이 없는 것 같다(왜냐하면 짠맛을 느끼기 어렵다) ---- chlorides

plot(quality~chlorides)
plot(quality~residual_sugar)
plot(quality~density)
plot(quality~alcohol)

corwhite <- cor(white)
corrplot(corwhite) # linear correlation degree
