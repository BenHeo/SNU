library(tidyverse)
library(data.table)
library(stringr)
movie <- read.csv("dat.csv")
head(movie, 2)
summary(movie)
names(movie)
table(movie$name)[table(movie$name) > 1]

movie <- movie %>% 
  mutate(country = as.factor(ifelse(country == "미국", 1, ifelse(country == "한국", 2, 0)))) # 1: 미국, 2: 한국, 3: 기타국


movie <- movie %>%
  mutate(magic7 = as.factor(ifelse(nchar(word(movie$name, 1, sep = ":|-|,|!")) <= 7 &
                                     nchar(word(movie$name, 1, sep = ":|-|,|!")) > 1, 1, 0))) # if less than or equal to 7 then 1

ggplot(movie) + geom_boxplot(aes(x=magic7, y=audience))

movie <- movie %>%
  mutate(main_actors = as.factor(ifelse(actor1 == "", 0, 1) + ifelse(actor2 == "", 0, 1) + ifelse(actor3 == "", 0, 1)))

ggplot(movie) + geom_boxplot(aes(x=main_actors, y=audience))

movie %>%
  gather(key, value, actor1:actor3) %>%
  filter(value != "")


movie$actor1 <- as.character(movie$actor1)
movie$actor2 <- as.character(movie$actor2)
movie$actor3 <- as.character(movie$actor3)

movie %>%
  filter(actor1 %like% "\\?" | actor2 %like% "\\?" | actor3 %like% "\\?")

# use gsub
movie %>%
  gather(key, value, actor1:actor3) %>%
  filter(value != "") %>%
  group_by(value) %>%
  summarise(actor_power = mean(audience)) %>%
  select(value, actor_power) %>%
  View()

movie %>%
  filter(actor1 == '휴잭맨' | actor2 == '휴잭맨' | actor3 == '휴잭맨')



as.character(movie$actor1)






lm.fit <- lm(audience~.-name, data = movie)
summary(lm.fit)
lm.pred <- predict(lm.fit, movie)
