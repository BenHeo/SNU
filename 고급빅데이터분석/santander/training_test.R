library(tidyverse)
library(xgboost)
library(data.table)
library(Matrix)
library(randomForest)

tttrain <- fread('san_2016_04.csv')
ttvalide <- fread('san_2016_05.csv')
f_col_names <- c("ind_empleado", "sexo", "age", "tiprel_1mes", "indresi", "indext", "indfall", "nomprov", "segmento")
tttrain[,c("ind_empleado", "sexo", "age", "tiprel_1mes", "indresi", "indext", "indfall", "nomprov", "segmento")] <- 
  as.data.frame(sapply(as.data.frame(tttrain)[, f_col_names], factor))
ttvalide[,c("ind_empleado", "sexo", "age", "tiprel_1mes", "indresi", "indext", "indfall", "nomprov", "segmento")] <- 
  as.data.frame(sapply(as.data.frame(ttvalide)[, f_col_names], factor))


x.train <- tttrain %>%
  select(-(ind_ahor_fin_ult1:ind_recibo_ult1))
y.train <- tttrain %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1)

x.test <- ttvalide %>%
  select(-(ind_ahor_fin_ult1:ind_recibo_ult1))
y.test <- ttvalide %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1)

dtrain <- xgb.DMatrix(data = Matrix(x.train, sparse = TRUE), label = as.matrix(y.train))
bstDMatrix <- xgboost(data = (x.train), label = y.train, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "multi:softprob")

#################################################################
tt_for_rf <- tttrain %>%
  select(int_date:segmento, ind_cco_fin_ult1)
tval_for_rf <- ttvalide %>%
  select(int_date:segmento)

tt.rf <- randomForest(as.factor(ind_cco_fin_ult1)~., tt_for_rf, ntree = 100)
tt.pred <- predict(tt.rf, tval_for_rf)


#################################################################
