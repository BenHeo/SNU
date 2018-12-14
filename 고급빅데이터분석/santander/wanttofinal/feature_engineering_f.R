library(tidyverse)
library(data.table)
library(readr)
library(lubridate)
memory.limit(size = 16000000)

santander <- fread('train_ver2.csv')


# 1.0, 2.0 등이 1, 2 등과 구별되어 있어서 같게 해주는 과정
santander$indrel_1mes[santander$indrel_1mes == "1.0"] <- "1"
santander$indrel_1mes[santander$indrel_1mes == "2.0"] <- "2"
santander$indrel_1mes[santander$indrel_1mes == "3.0"] <- "3"
santander$indrel_1mes[santander$indrel_1mes == "4.0"] <- "4"

# age 결측치에 대해선 중앙값 처리를 해주었다
# age에 이상치도 있고 굳이 연속형 변수를 쓰는 것이 무의미하다고 여겨져 설명력 있게 이산형 변수로 변환, 60대 이상을 묶은 것은 퀀타일상 60대 이상의 수가 많지 않기 때문
santander$age[is.na(santander$age)] <- median(santander$age, na.rm = TRUE)

santander <- santander %>%
  mutate(age = ifelse(age < 30, "20s", ifelse(age < 40, "30s", 
                                              ifelse(age < 50, "40s", ifelse(age < 60, "50s", "60above")))))

# ind_empleado에서 빈 값은 가장 많은 N으로 처리했다
santander$ind_empleado[santander$ind_empleado == ""] <- "N"

# ind_nuevo가 비어있는 사람들의 지난 기록을 보니 최대가 6인 것으로 보아 결측치는 모두 신규 회원들이다
santander$ind_nuevo[is.na(santander$ind_nuevo)] <- 1

# antiguedad가 결측치인 경우 ind_nuevo가 모두 1인 것으로 보아 모두 신규회원이다
# 결측치인 것이 가입한지 얼마 안 되어서 그럴 수 있다는 생각으로 1로 처리하기로 했다
table(santander[is.na(santander$antiguedad), "ind_nuevo"])
santander$antiguedad[is.na(santander$antiguedad)] <- 1
santander$antiguedad[santander$antiguedad < 0] <- 0
ggplot(santander, aes(x=antiguedad)) + geom_histogram()

# tipodom은 결측치 제외 모두 1이고, 전체 수 대비 결측치 수가 적어서 variance가 없다고 판단하여 제거
# cod_prov는 nomprov와 의미상 겹치기에 제거
# 명목형 변수지만 너무 많은 canal_entrada는 활용 불가라 생각해 제거
# 결측치가 결측이 아닌 데이터에 비해 압도적으로 많은 conyuemp 제거
# test set도 cleanse 작업을 하고 나니 결측치에서 train과 test에서 차이가 있음을 발견
# 특히 ult_fec_cli_1t의 경우 test set의 대부분이 결측치여서 train을 한다고 한들 test에서 쓸 수 없음
santander <- santander %>%
  select(-tipodom, -cod_prov, -canal_entrada, -conyuemp, -pais_residencia, -ult_fec_cli_1t)

# CORU횗A, A를 없애는 작업
santander$nomprov[santander$nomprov == "CORU횗A, A"] <- "CORUNA, A"
unique(santander$nomprov)
santander$nomprov[santander$nomprov == ""] <- "UNKNOWN"

# sexo는 알 수 없으므로 unknown
santander$sexo[santander$sexo == ""] <- "UNKNOWN"

# 지역별로 수익에 차이가 있으므로 지역별 median 값으로 replace
santander %>%
  group_by(nomprov) %>% 
  summarise(med_renta = median(renta, na.rm = TRUE)) %>%
  ggplot(aes(x=nomprov, y=med_renta)) + geom_point()

med_per_prov <- santander %>%
  group_by(nomprov) %>% 
  summarise(med_renta = median(renta, na.rm = TRUE))

santander[is.na(santander$renta), c("renta")] <- santander[is.na(santander$renta), c("renta", "nomprov")] %>%
  left_join(med_per_prov) %>%
  mutate(renta = med_renta) %>%
  select(renta)

# ind_nomina_ult1과 ind_nom_pens_ult1은 알기 어렵기 때문에 보수적으로 0으로 취한다
santander$ind_nomina_ult1[is.na(santander$ind_nomina_ult1)] <- 0
santander$ind_nom_pens_ult1[is.na(santander$ind_nom_pens_ult1)] <- 0

# indrel도 결측치를 1로
santander$indrel[is.na(santander$indrel)] <- 1

#i ind_actividad_cliente는 0/1이 비슷한 규모이기 때문에 "UNKNOWN" 해야 하지만 후에 0/1로 변화 봐야 하므로 더 많은 0으로 대체
santander$ind_actividad_cliente[is.na(santander$ind_actividad_cliente)] <- 0

# indrel_1mes도 보수적으로 결측치는 1로, P값은 P 대신 5를 사용해서 표기
santander$indrel_1mes[is.na(santander$indrel_1mes)] <- "1"
santander$indrel_1mes[santander$indrel_1mes == ""] <- "1"
santander$indrel_1mes[santander$indrel_1mes == "P"] <- "5"
santander$indrel_1mes <- as.factor(santander$indrel_1mes)

# tiprel_1mes도 보수적으로 결측치는 "I"
santander$tiprel_1mes[santander$tiprel_1mes == ""] <- "I"

# segment는 보수적으로 가장 많은 02- particulares
santander$segmento[santander$segmento == ""] <- "02 - PARTICULARES"

# 가장 최근 primary customer였던 ult_fec_cli_1t는 Date 형식이므로 결측값을 "UNKNOWN"
# santander$ult_fec_cli_1t[santander$ult_fec_cli_1t == ""] <- "UNKNOWN"

# indext, indfall는 N이 훨씬 많으므로 N으로 결측치 채운다
santander$indext[santander$indext == ""] <- "N"
santander$indfall[santander$indfall == ""] <- "N"

# indresi는 S가 훨씬 많으므로 N으로 결측치 채운다
santander$indresi[santander$indresi == ""] <- "S"

# fecha_alta는 median 값으로 대체한다
santander$fecha_alta[santander$fecha_alta == ""] <- median(santander$fecha_alta, na.rm=TRUE)


santander <- santander %>%
  mutate(mth= month(fecha_dato),
         y = year(fecha_dato),
         int_date = (y-2015)*12 + mth,
         c_month = month(fecha_alta),
         c_year = year(fecha_alta),
         diff_month = 12*(y-c_year) + (mth-c_month)) %>%
  select(-mth, -y, -c_month, -c_year, -fecha_alta)
santander <- santander %>%
  select(int_date, diff_month, ncodpers:ind_recibo_ult1)
santander <- santander %>%
  mutate(prev_date = int_date - 1) %>%
  select(int_date, prev_date, diff_month:ind_recibo_ult1)

features <- grepl("ind_+.*ult.*",names(santander))
santander[,features] <- lapply(santander[,features],function(x)as.integer(round(x)))
int_cols <- c("int_date", "prev_date", "diff_month", "ind_nuevo", "antiguedad", "indrel", "ind_actividad_cliente")
santander[,int_cols] <- lapply(santander[,int_cols],function(x)as.integer(round(x)))

################################
fwrite(santander, "cleansed_train.csv")
saveRDS(santander, file = "my_santander.rds")
# santander <- readRDS(file = "my_santander.rds")

rm(santander)
gc()




#####################################################################################
# Same code for test_set
#####################################################################################

san_test <- fread('test_ver2.csv')


# 1.0, 2.0 등이 1, 2 등과 구별되어 있어서 같게 해주는 과정
san_test$indrel_1mes[san_test$indrel_1mes == "1.0"] <- "1"
san_test$indrel_1mes[san_test$indrel_1mes == "2.0"] <- "2"
san_test$indrel_1mes[san_test$indrel_1mes == "3.0"] <- "3"
san_test$indrel_1mes[san_test$indrel_1mes == "4.0"] <- "4"

# age 결측치에 대해선 중앙값 처리를 해주었다
# age에 이상치도 있고 굳이 연속형 변수를 쓰는 것이 무의미하다고 여겨져 설명력 있게 이산형 변수로 변환, 60대 이상을 묶은 것은 퀀타일상 60대 이상의 수가 많지 않기 때문
san_test$age[is.na(san_test$age)] <- median(san_test$age, na.rm = TRUE)

san_test <- san_test %>%
  mutate(age = ifelse(age < 30, "20s", ifelse(age < 40, "30s", 
                                              ifelse(age < 50, "40s", ifelse(age < 60, "50s", "60above")))))

# ind_empleado에서 빈 값은 가장 많은 N으로 처리했다
san_test$ind_empleado[san_test$ind_empleado == ""] <- "N"

# ind_nuevo가 비어있는 사람들의 지난 기록을 보니 최대가 6인 것으로 보아 결측치는 모두 신규 회원들이다
san_test$ind_nuevo[is.na(san_test$ind_nuevo)] <- 1

# antiguedad가 결측치인 경우 ind_nuevo가 모두 1인 것으로 보아 모두 신규회원이다
# 결측치인 것이 가입한지 얼마 안 되어서 그럴 수 있다는 생각으로 1로 처리하기로 했다
table(san_test[is.na(san_test$antiguedad), "ind_nuevo"])
san_test$antiguedad[is.na(san_test$antiguedad)] <- 1
san_test$antiguedad[san_test$antiguedad < 0] <- 0
ggplot(san_test, aes(x=antiguedad)) + geom_histogram()

# tipodom은 결측치 제외 모두 1이고, 전체 수 대비 결측치 수가 적어서 variance가 없다고 판단하여 제거
# cod_prov는 nomprov와 의미상 겹치기에 제거
# 명목형 변수지만 너무 많은 canal_entrada는 활용 불가라 생각해 제거
# 결측치가 결측이 아닌 데이터에 비해 압도적으로 많은 conyuemp 제거
# test set도 cleanse 작업을 하고 나니 결측치에서 train과 test에서 차이가 있음을 발견
# 특히 ult_fec_cli_1t의 경우 test set의 대부분이 결측치여서 train을 한다고 한들 test에서 쓸 수 없음
# 따라서 train, test에서 모두 지우는 것이 요망됨
san_test <- san_test %>%
  select(-tipodom, -cod_prov, -canal_entrada, -conyuemp, -pais_residencia, -ult_fec_cli_1t)

# CORU횗A, A를 없애는 작업
san_test$nomprov[san_test$nomprov == "CORU횗A, A"] <- "CORUNA, A"
unique(san_test$nomprov)
san_test$nomprov[san_test$nomprov == ""] <- "UNKNOWN"

# sexo는 알 수 없으므로 unknown
san_test$sexo[san_test$sexo == ""] <- "UNKNOWN"

# 지역별로 수익에 차이가 있으므로 지역별 median 값으로 replace
san_test %>%
  group_by(nomprov) %>% 
  summarise(med_renta = median(renta, na.rm = TRUE)) %>%
  ggplot(aes(x=nomprov, y=med_renta)) + geom_point()

san_test[is.na(san_test$renta), c("renta")] <- san_test[is.na(san_test$renta), c("renta", "nomprov")] %>%
  left_join(med_per_prov) %>%
  mutate(renta = med_renta) %>%
  select(renta)

# ind_nomina_ult1과 ind_nom_pens_ult1은 알기 어렵기 때문에 보수적으로 0으로 취한다
san_test$ind_nomina_ult1[is.na(san_test$ind_nomina_ult1)] <- 0
san_test$ind_nom_pens_ult1[is.na(san_test$ind_nom_pens_ult1)] <- 0

# indrel도 결측치를 1로
san_test$indrel[is.na(san_test$indrel)] <- 1

#i ind_actividad_cliente는 0/1이 비슷한 규모이기 때문에 "UNKNOWN" 해야 하지만 후에 0/1로 변화 봐야 하므로 더 많은 0으로 대체
san_test$ind_actividad_cliente[is.na(san_test$ind_actividad_cliente)] <- 0

# indrel_1mes도 보수적으로 결측치는 1로, P값은 P 대신 5를 사용해서 표기
san_test$indrel_1mes[is.na(san_test$indrel_1mes)] <- "1"
san_test$indrel_1mes[san_test$indrel_1mes == ""] <- "1"
san_test$indrel_1mes[san_test$indrel_1mes == "P"] <- "5"
san_test$indrel_1mes <- as.factor(san_test$indrel_1mes)

# tiprel_1mes도 보수적으로 결측치는 "I"
san_test$tiprel_1mes[san_test$tiprel_1mes == ""] <- "I"

# segment는 보수적으로 가장 많은 02- particulares
san_test$segmento[san_test$segmento == ""] <- "02 - PARTICULARES"

# 가장 최근 primary customer였던 ult_fec_cli_1t는 Date 형식이므로 결측값을 "UNKNOWN"
# san_test$ult_fec_cli_1t[san_test$ult_fec_cli_1t == ""] <- "UNKNOWN"

# indext, indfall는 N이 훨씬 많으므로 N으로 결측치 채운다
san_test$indext[san_test$indext == ""] <- "N"
san_test$indfall[san_test$indfall == ""] <- "N"

# indresi는 S가 훨씬 많으므로 N으로 결측치 채운다
san_test$indresi[san_test$indresi == ""] <- "S"

# fecha_alta는 median 값으로 대체한다
san_test$fecha_alta[san_test$fecha_alta == ""] <- median(san_test$fecha_alta, na.rm=TRUE)

san_test <- san_test %>%
  mutate(mth= month(fecha_dato),
         y = year(fecha_dato),
         int_date = (y-2015)*12 + mth,
         c_month = month(fecha_alta),
         c_year = year(fecha_alta),
         diff_month = 12*(y-c_year) + (mth-c_month)) %>%
  select(-mth, -y, -c_month, -c_year, - fecha_alta)
san_test <- san_test %>%
  select(int_date, diff_month, ncodpers:segmento)
san_test <- san_test %>%
  mutate(prev_date = int_date - 1) %>%
  select(int_date, prev_date, diff_month:segmento)

int_cols <- c("int_date", "prev_date", "diff_month", "ind_nuevo", "antiguedad", "indrel", "ind_actividad_cliente")
san_test[,int_cols] <- lapply(san_test[,int_cols],function(x)as.integer(round(x)))

################################
fwrite(san_test, "cleansed_test.csv")
saveRDS(san_test, file = "my_santander_test.rds")
# san_test <- readRDS(file = "my_santander_test.rds")



# ########################################################################################
# # following http://alanpryorjr.com/2016-12-19-Kaggle-Competition-Santander-Solution
# library(fasttime)
# features <- names(santander)[grepl("ind_+.*ult.*",names(santander))]
# test_feats <- santander[1:929615,features]
# test_feats[test_feats == 1] <- 0
# san_test <- cbind(san_test, test_feats)
# df <- rbind(santander, san_test)
# saveRDS(df, "tot.rds")
# # df <- readRDS(file = "tot.rds")
# features <- grepl("ind_+.*ult.*",names(df))
# df[,features] <- lapply(df[,features],function(x)as.integer(round(x)))
# int_cols <- c("int_date", "prev_date", "diff_month", "ind_nuevo", "antiguedad", "indrel", "ind_actividad_cliente")
# df[,int_cols] <- lapply(df[,int_cols],function(x)as.integer(round(x)))
# 
# 
# # make lags
# source('create-lag-feature.R')
# df <- as.data.table(df)
# df <- create.lag.feature(df,'ind_actividad_cliente',1:11,na.fill=0)
# 
# lagged_features <- grepl("lagged.ind_+.*months.*",names(df))
# df[,lagged_features] <- lapply(df[,lagged_features],function(x)as.integer(x))
# 
# test <- df %>%
#   filter(int_date == max(df$int_date))
# df <- df %>%
#   filter(int_date < max(df$int_date))
# 
# fwrite(df,"cleaned_train.csv",row.names=FALSE)
# saveRDS(df, "cleaned_train.rds")
# # train <- readRDS(file = "cleaned_train.rds")
# fwrite(test,"cleaned_test.csv",row.names=FALSE)
# saveRDS(test, "cleaned_test.rds")
# # test <- readRDS(file = "cleaned_test.rds")


# 
# 
# train$ind_actividad_cliente[train$ind_actividad_cliente=="UNKNOWN"] <- 0
# train[,"ind_actividad_cliente"] <- sapply(train[,"ind_actividad_cliente"], function(x) as.integer(x))
# 
# test$ind_actividad_cliente[test$ind_actividad_cliente=="UNKNOWN"] <- 0
# test[,"ind_actividad_cliente"] <- sapply(test[,"ind_actividad_cliente"], function(x) as.integer(x))
