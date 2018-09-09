library(tidyverse)
library(glmnet)
dat <- read.csv("mobility.csv")
apply(is.na(dat), 2, sum)
dat$State <- as.factor(dat$State) # factorization
dat$Urban <- as.factor(dat$Urban) # factorization
dat$Violent_crime[is.na(dat$Violent_crime)] <- 0
dat <- dat %>%
  filter(!is.na(Mobility))

################### 1 #########################

ggplot(dat) + geom_point(aes(x = Longitude, y = Latitude, color = Mobility)) + 
  scale_color_gradient()
dat %>%
  mutate(MobilityCat = ifelse(Mobility > 0.1, "high", "low")) %>%
  filter(State != "HI", State != "AK") %>%
  ggplot() + geom_point(aes(x = Longitude, y = Latitude, color = MobilityCat))+ 
  scale_color_grey(start=0.1, end = 0.7)

#################### 2 #######################3

lm.fit <- lm(Mobility~Population, data=dat)
ggplot(dat) + geom_point(aes(x=Population, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients # need log scale

lm.fit <- lm(Mobility~Income, data=dat)
ggplot(dat) + geom_point(aes(x=Income, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients

lm.fit <- lm(Mobility~Seg_racial, data=dat)
ggplot(dat) + geom_point(aes(x=Seg_racial, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients

lm.fit <- lm(Mobility~Share01, data=dat)
ggplot(dat) + geom_point(aes(x=Share01, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients # need log scale or remove outlier

lm.fit <- lm(Mobility~School_spending, data=dat)
ggplot(dat) + geom_point(aes(x=School_spending, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients

lm.fit <- lm(Mobility~Violent_crime, data=dat)
ggplot(dat) + geom_point(aes(x=Violent_crime, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients # discrete

lm.fit <- lm(Mobility~Commute, data=dat)
ggplot(dat) + geom_point(aes(x=Commute, y=Mobility)) + 
  geom_abline(intercept = lm.fit$coefficients[1], slope = lm.fit$coefficients[2], color = "pink", size = 2)
lm.fit$coefficients

################# 3 ###############################

lm_appro <- lm(Mobility~.-ID-Name-State-Longitude-Latitude-HS_dropout-Student_teacher_ratio
               , data=dat) # not appropriate or NA
summary(lm_appro)
coeffs <- summary(lm_appro)$coef[,1:2]
coeffs
coeffs[c("Population", "Income", "Seg_racial", "Share01", "School_spending", "Violent_crime", "Commute"),]

library(car)
vif_appro <- vif(lm_appro)
vif_appro[vif_appro > 10] # ridge or variable elimination

################# 4 ############################

apply(is.na(dat[,c("Colleges", "Tuition", "Graduation")]), 2, sum)
ggplot(dat) + geom_point(aes(x=Population, y=Mobility, color = Colleges)) + 
  scale_color_gradient(na.value = "red")
ggplot(dat) + geom_point(aes(x=Population, y=Mobility, color = Tuition)) + 
  scale_color_gradient(na.value = "red")
ggplot(dat) + geom_point(aes(x=Population, y=Mobility, color = Graduation)) + 
  scale_color_gradient(na.value = "red")


dat[,c("Colleges", "Tuition", "Graduation")][is.na(dat[,c("Colleges", "Tuition", "Graduation")])] <- 0
dat <- dat %>%
  mutate(HE = ifelse(Colleges == 0 & Tuition == 0 & Graduation ==0, 0, 1))

################### 5 ###########################

dat <- dat %>%
  mutate(logMobil=log(Mobility))
flm.fit <- lm(logMobil~Population+Seg_poverty+Commute+Gini+Manufacturing+
                Religious+Violent_crime+Single_mothers+Progressivity,
              data=dat)
summary(flm.fit)
fcoeff <- summary(flm.fit)$coef
fvif <- vif(flm.fit)
fvif[fvif > 10]
fcoeff[,1:2]

########################## 6 ########################

dat$logpred <- predict(flm.fit)
dat$pred <- exp(dat$logpred)
ggplot(dat) + geom_point(aes(x = Longitude, y = Latitude, color = pred)) + 
  scale_color_gradient()

ggplot(dat) + geom_point(aes(x = Longitude, y = Latitude, color = Mobility)) + 
  scale_color_gradient()

######################### 7 ########################

Pitts <- dat %>%
  filter(State == "PA")

Pitts %>%
  select(Mobility, pred)

bind_cols(double = Pitts$pred * exp(fcoeff["Violent_crime",1]*Pitts$Violent_crime),
          halve = Pitts$pred / exp(fcoeff["Violent_crime",1]*Pitts$Violent_crime/2))


exp(predict(flm.fit, Pitts[,names(flm.fit$coefficients)[-1]], level=0.95, interval="confidence"))
exp(predict(flm.fit, Pitts[,names(flm.fit$coefficients)[-1]], level=0.95, interval="prediction"))

######################### 8 ########################

dat <- dat %>%
  mutate(residual = Mobility - pred,
         logres = logMobil - logpred)

g <- ggplot(dat) + geom_point(aes(x = Longitude, y = Latitude, color = (residual))) 

top5 <- dat %>% 
  arrange(desc(residual)) %>%
  head(5) %>%
  mutate(group = 1)

bottom5 <- dat %>% 
  arrange(residual) %>%
  head(5) %>%
  mutate(group = 2)

tb5 <- bind_rows(top5, bottom5)
g + 
  geom_label(data=tb5, aes(x = Longitude, y = Latitude, label=Name, fill = factor(group)), size = 5, parse = TRUE)


######################### 9 ########################

ggplot(dat, aes(x = Mobility, y = pred)) + geom_point() + geom_smooth(method = "lm")
ggplot(dat, aes(x = logMobil, y = logpred)) + geom_point() + geom_smooth(method = "lm")
ggplot(dat, aes(x = Mobility, y = residual)) + geom_point() + geom_smooth(method = "lm")
ggplot(dat, aes(x = logMobil, y = logres)) + geom_point() + geom_smooth(method = "lm")


######################## 10 #######################

mmdat <- dat[,c("Middle_class", "Mobility")]
mmdat <- mmdat %>% filter(!is.na(Middle_class))
nlm.fit <- lm(Mobility~Middle_class, data = mmdat)
summary(nlm.fit)
ggplot(mmdat, aes(Middle_class, Mobility)) + geom_point() + 
  geom_abline(intercept = nlm.fit$coefficients[1], slope = nlm.fit$coefficients[2], color = "red")
interval_conf <- predict(nlm.fit, interval = "confidence")
interval_pred <- predict(nlm.fit, interval = "prediction")
mmdatinter <- bind_cols(mmdat, as.data.frame(interval_conf), pred_lwr = interval_pred[,2], pred_upr = interval_pred[,3])
ggplot(mmdatinter, aes(Middle_class, Mobility)) + geom_point() + 
  geom_line(aes(y=fit), color = "red") +
  geom_line(aes(y=upr), color = "blue") +
  geom_line(aes(y=lwr), color = "blue") +
  geom_line(aes(y=pred_upr), color = "green") +
  geom_line(aes(y=pred_lwr), color = "green")

library(boot)

boot.conf <- function(data, indices){
  data <- data[indices,]
  lm.fit <- lm(Mobility~Middle_class, data = data)
  interval_conf <- predict(lm.fit, interval = "confidence")
  mean(interval_conf)
}
boot.out <- boot(mmdat, boot.conf, 100)
boot.out
boot.ci(boot.out, type = c("norm", "perc"))

boot.pred <- function(data, indices){
  data <- data[indices,]
  lm.fit <- lm(Mobility~Middle_class, data = data)
  interval_pred <- predict(lm.fit, interval = "prediction")
  mean(interval_pred)
}
boot.out <- boot(mmdat, boot.pred, 100)
boot.out
boot.ci(boot.out, type = c("norm", "perc"))

ssfit <- smooth.spline(mmdat$Middle_class, mmdat$Mobility, cv = TRUE)
ssfit

boot.ss <- function(data, indices){
  data <- data[indices,]
  ssfit <- smooth.spline(data$Middle_class, data$Mobility, df = 5.056275)
  mean(ssfit$y)
}
ssboot <- boot(mmdat, boot.ss, 100)
ssboot
boot.ci(ssboot, type = c("norm", "perc"))
ssci <- boot.ci(ssboot, type = c("perc"))$percent[c(4,5)]
ssci

ssdf = bind_cols(x=ssfit$x, y=ssfit$y, lwr = ssfit$y - ssci[1], upr = ssfit$y + ssci[2])

ggplot(ssdf, aes(x, y)) + geom_point() + geom_line(aes(y=lwr), color = "blue") +
  geom_line(aes(y=upr), color = "green")



