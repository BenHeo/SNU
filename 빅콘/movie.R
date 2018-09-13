library(tidyverse)
library(data.table)
library(stringr)
library(plotly)
library(caret)
library(lubridate)
library(randomForest)
library(SuperLearner)
library(gbm)
movie <- read.csv("dat.csv")
head(movie, 2)
summary(movie)
names(movie)
table(movie$name)[table(movie$name) > 1]

movie <- movie %>%
  mutate(small_audience = audience/10000)
rmse <- function(real, pred){
  rmse <- sqrt(mean((real-pred)^2))
  rmse
}

# ggplot(movie) + geom_histogram(aes(audience))
# ggplot(movie) + geom_histogram(aes(log(audience)))
# ggplot(movie) + geom_histogram(aes(sqrt(log(audience))))
# ggplot(movie) + geom_histogram(aes(sqrt(audience)))


ggplot(movie) + geom_bar(aes(year))
taud <- sum(movie$audience)

movie$year <- as.numeric(movie$year)
year_impact <- movie %>%
  filter(year != 2018) %>%
  group_by(year) %>%
  summarise(n = n(),
            t_per_year = sum(audience), 
            standard_aud = t_per_year/n) %>%
  select(year, standard_aud) %>%
  mutate(mu = mean(standard_aud),
         ratio = standard_aud/mu) %>%
  select(year, ratio)
year_impact$year <- as.numeric(year_impact$year)
year_impact <- bind_rows(year_impact, data.frame(year = 2018, ratio = 1.15))

ggplot(movie, aes(audience)) + geom_histogram(bins=30)

# overmovie <- movie[movie$audience > 1200000,]
# 
# for (i in 1:3){
#   movie <- bind_rows(movie, overmovie)
# }
# ggplot(movie, aes(audience)) + geom_histogram(bins=30)
# 
# overmovie <- movie[movie$audience > 3500000,]
# 
# for (i in 1:4){
#   movie <- bind_rows(movie, overmovie)
# }
# ggplot(movie, aes(audience)) + geom_histogram(bins=30) 


movie$month <- as.factor(movie$month)

movie <- movie %>% 
  mutate(country = as.factor(ifelse(country == "미국", 1, ifelse(country == "한국", 2, 0)))) # 1: 미국, 2: 한국, 3: 기타국

movie$genre <- as.character(movie$genre)
movie = movie[movie$genre != '서부극(웨스턴)' & movie$genre != '공연' & movie$genre != '뮤지컬' & movie$genre != '다큐멘터리',]

ggplot(movie, aes(small_audience)) + geom_histogram(bins=30) 



# g <- ggplot(movie) + geom_density(aes(x=audience, color=month))
# ggplotly(g)
# g <- ggplot(movie) + geom_density(aes(x=audience, color=genre))
# ggplotly(g)

# 장르1 = 가족, 공포(호러), 애니메이션 
# 장르2 = SF, 드라마, 멜로/로맨스, 미스터리, 스릴러, 코미디
# 장르3 = 범죄, 사극, 액션, 어드벤쳐, 전쟁, 판타지
movie$genre[movie$genre ==  '가족' | movie$genre ==  '공포(호러)' | movie$genre ==  '애니메이션'] = '장르1'
movie$genre[movie$genre ==  'SF' | movie$genre ==  '드라마' | movie$genre ==  '멜로/로맨스' | movie$genre == '미스터리' | 
              movie$genre == '스릴러' | movie$genre == '코미디'] = '장르2'
movie$genre[movie$genre ==  '범죄' | movie$genre ==  '사극' | movie$genre ==  '액션' | movie$genre ==  '어드벤처' | 
              movie$genre ==  '전쟁' | movie$genre ==  '판타지'] = '장르3'
movie$genre <- as.factor(movie$genre)

# director1 <- movie %>% 
#   group_by(director) %>%
#   summarise(n= n()) %>%
#   filter(n<=1)
# 
# g <- ggplot(movie[!(movie$director %in% director1$director),]) + geom_density(aes(x=audience, color=director))
# ggplotly(g)

x = movie %>%
  select(screen, audience, genre, limit, star)
  
hc.average=hclust(dist(x), method="average")


movie = movie %>% group_by(director) %>% mutate(director_score = log(sum(audience)))
movie = movie %>% group_by(producer) %>% mutate(producer_score = log(sum(audience)))
movie = movie %>% group_by(Importer) %>% mutate(Importer_score = log(sum(audience)))
movie = movie %>% group_by(Distributor) %>% mutate(Distributor_score = log(sum(audience)))

movie$name <- as.character(movie$name)
movie <- movie %>%
  mutate(magic7 = ifelse(nchar(word(name, 1, sep = ":|-|,|!")) <= 7 &
                                     nchar(word(name, 1, sep = ":|-|,|!")) > 1, 1, 0)) # if less than or equal to 7 then 1
movie$magic7 <- as.factor(movie$magic7)
ggplot(movie) + geom_boxplot(aes(x=magic7, y=audience))

movie$actor1 <- as.character(movie$actor1)
movie$actor2 <- as.character(movie$actor2)
movie$actor3 <- as.character(movie$actor3)

movie <- movie %>%
  mutate(main_actors = ifelse(actor1 == "", 0, 1) + ifelse(actor2 == "", 0, 1) + ifelse(actor3 == "", 0, 1))

movie$main_actors <- as.factor(movie$main_actors)
ggplot(movie) + geom_boxplot(aes(x=main_actors, y=audience))

movie$star = ifelse(movie$star >= 8, 1, 0)
movie$month = ifelse(movie$month %in% c(5,7,8,12), 1, 0)
movie$month <- as.factor(movie$month)
movie$star <- as.factor(movie$star)
movie$year <- as.factor(movie$year)

# use gsub
ap <- movie %>%
  gather(key, value, actor1:actor3) %>%
  filter(value != "") %>%
  group_by(value) %>%
  summarise(actor_power = sum(audience)) %>%
  select(value, actor_power)



movie <- movie %>%
  left_join(ap, by = c("actor1" = "value"), suffix = c('', '1')) %>%
  left_join(ap, by = c("actor2" = "value"), suffix = c('', '2')) %>%
  left_join(ap, by = c("actor2" = "value"), suffix = c('', '3')) %>%
  rename(actor_power1 = actor_power) %>%
  mutate(actors_value = log((actor_power1 + ifelse(is.na(actor_power2), 0, actor_power2)
                         + ifelse(is.na(actor_power3), 0, actor_power3))/
           (1 + ifelse(is.na(actor_power2), 0, 1) + ifelse(is.na(actor_power3), 0, 1)))) # 1번 배우에 0.5, 2번 배우에 0.3, 3번 배우에 0.2 비중




# lm.fit <- lm(audience~., data = movie)
# step(lm.fit, direction = "both")
lm.fit <- lm(small_audience~month+country+screen+genre+limit+star+director_score+producer_score+Importer_score+
                 Distributor_score+magic7+main_actors*actors_value, data = movie)
summary(lm.fit)
lm.pred <- predict(lm.fit)
rmse(movie$small_audience, lm.pred)
ggplot(bind_cols(real = movie$small_audience, pred = lm.pred), aes(x=real, y=pred)) + geom_point() + geom_smooth()
ggplot(bind_cols(real = movie$small_audience, res = lm.fit$residuals), aes(real, res)) + geom_point() + geom_smooth()
glm.fit <- glm(small_audience~month+country+screen+genre+limit+star+director_score+producer_score+Importer_score+
                 Distributor_score+main_actors*actors_value, data = movie, family = "poisson")
summary(glm.fit)
# step(glm.fit, direction = "both")
glm.pred <- predict(glm.fit, type = "response")
rmse(movie$small_audience, glm.pred)
head(bind_cols(real = movie$small_audience, pred = glm.pred))

ggplot(bind_cols(real = movie$small_audience, pred = glm.pred), aes(x=real, y=pred)) + geom_point() + geom_smooth()


rf <- randomForest(small_audience~month+country+screen+genre+limit+star+director_score+producer_score+Importer_score+
               Distributor_score+magic7+main_actors*actors_value, data = movie, ntree = 1000, maxnodes = 6)
rf.pred <- predict(rf)
rmse(movie$small_audience, rf.pred)
ggplot(bind_cols(real = movie$small_audience, pred = rf.pred), aes(x=real, y=pred)) + geom_point() + geom_smooth()
importance(rf)


preds <- data.frame(orgn = as.numeric(movie$audience),l = lm.pred, gg = glm.pred, raf = rf.pred)
preds.fit <- lm(origin~gg+raf, data=preds)
SuperLearner(Y = preds$orgn)


gbm.fit <- gbm(audience~month+country+screen+genre+limit+star+director_score+producer_score+Importer_score+
                 Distributor_score+magic7+main_actors+actors_value, data = movie, distribution = "poisson",
               shrinkage = 0.01, interaction.depth = 4, n.trees = 1000, cv.folds = 10)
gbm.fit
summary(gbm.fit)
plot(gbm.fit, i="actors_value") 
gbm.pred <- predict(gbm.fit, n.trees = 1000)
rmse(movie$audience, gbm.pred)




library(pls)
small_mov <- movie %>%
  select(audience,month,country,screen,genre,limit,star,director_score,producer_score,Importer_score,
           Distributor_score,magic7,main_actors,actors_value)
x=model.matrix(audience ~.-Distributor,small_mov)[, -1]
y=small_mov$audience
pcr.fit <- pcr(unlist(y)~x, scale=T, ncomp=2)
rmse(pcr.fit$fitted.values, movie$audience)






Fold_index <- createFolds(1:nrow(movie), k = 10)
result = 0
for(k in 1:10){
  Train <- movie[-Fold_index[[k]],]
  Test <- movie[Fold_index[[k]],]
  rf <- randomForest(audience~month+country+screen+genre+limit+star+director_score+producer_score+Importer_score+
                       Distributor_score+magic7+main_actors*actors_value, data = Train, ntree = 1000, maxnodes = 6)
  rf.pred <- predict(rf, Test)
  
  result[[k]] <- rmse(rf.pred, Test$audience)
}
mean(result)









#############################################################

dat = read.csv('dat.csv', stringsAsFactors = F)
dat = dat[dat$genre != '서부극(웨스턴)' & dat$genre != '공연' & dat$genre != '뮤지컬' & dat$genre != '다큐멘터리',]
dat$country[dat$country != '미국' & dat$country != '한국'] = '제3국'

dat$genre[dat$genre ==  '가족' | dat$genre ==  '공포(호러)' | dat$genre ==  '애니메이션'] = '장르1'
dat$genre[dat$genre ==  'SF' | dat$genre ==  '드라마' | dat$genre ==  '멜로/로맨스' | dat$genre == '미스터리' | 
            dat$genre == '스릴러' | dat$genre == '코미디'] = '장르2'
dat$genre[dat$genre ==  '범죄' | dat$genre ==  '사극' | dat$genre ==  '액션' | dat$genre ==  '어드벤처' | 
            dat$genre ==  '전쟁' | dat$genre ==  '판타지'] = '장르3'

dat = dat %>% group_by(director) %>% mutate(director_score = mean(audience))
dat = dat %>% group_by(producer) %>% mutate(producer_score = mean(audience))
dat = dat %>% group_by(Importer) %>% mutate(Importer_score = mean(audience))
dat = dat %>% group_by(Distributor) %>% mutate(Distributor_score = mean(audience))
dat = dat %>% group_by(actor1) %>% mutate(actor1_score = mean(audience))
dat = dat %>% group_by(actor2) %>% mutate(actor2_score = mean(audience))
dat = dat %>% group_by(actor3) %>% mutate(actor3_score = mean(audience))
dat$star = ifelse(dat$star >= 8, 1, 0)
dat$month = ifelse(dat$month %in% c(5,7,8,12), 1, 0)

dat = dat[,-c(1:6,14:16)]
dat$year <- as.factor(dat$year)
dat$month <- as.factor(dat$month)
dat$country <- as.factor(dat$country)
dat$star <- as.factor(dat$star)
dat$genre <- as.factor(dat$genre)
dat$limit <- as.factor(dat$limit)

Fold_index <- createFolds(1:nrow(dat), k = 10)
result = 0
for(k in 1:10){
  Train <- dat[-Fold_index[[k]],]
  Test <- dat[Fold_index[[k]],]
  out <- lm(audience~., data = Train)
  # car::vif(out)
  pred <- predict(out, Test, type='response')
  result[[k]] = sqrt(mean((pred - Test$audience)^2))
}
mean(result)
