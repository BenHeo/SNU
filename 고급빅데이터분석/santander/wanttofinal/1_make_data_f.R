library(data.table)
library(tidyverse)

# drop fecha_alta, ult_fec_cli_1t, tipodom, cod_prov, ind_ahor_fin_ult1, ind_aval_fin_ult1, ind_deco_fin_ult1 and ind_deme_fin_ult1
data <- fread("cleansed_train.csv", drop=c(20,21,29,30))

date.list <- c(unique(data$int_date), 18) # list of dates from 1 to 18
product.list <- colnames(data)[(ncol(data)-19):ncol(data)]  # list of products to recommend

##### data 1: inner join with last month #####

for(i in c(6,12:length(date.list))) {
  print(date.list[i])
  if(date.list[i] != 18) {
    out <- data[int_date==date.list[i]]
    out <- merge(out, data[int_date==date.list[i-1], c("ncodpers","tiprel_1mes","ind_actividad_cliente",product.list), with=FALSE], by="ncodpers", suffixes=c("","_last"))
    write.csv(out, paste0("train_", date.list[i], ".csv"), row.names=FALSE)
  } else {
    out <- fread("cleansed_test.csv")
    out <- merge(out, data[int_date==date.list[i-1], c("ncodpers","tiprel_1mes","ind_actividad_cliente",product.list), with=FALSE], by="ncodpers", suffixes=c("","_last"))
    colnames(out)[(ncol(out)-19):ncol(out)] <- paste0(colnames(out)[(ncol(out)-19):ncol(out)], "_last")
    write.csv(out, paste0("test_", date.list[i], ".csv"), row.names=FALSE)
  }
}

##### data 2: count the change of index #####

for(i in c(6,12:length(date.list))) {
  print(date.list[i])
  if(date.list[i] != 18) {
    out <- merge(data[int_date==date.list[i], .(ncodpers)], data[int_date==date.list[i-1], .(ncodpers)], by="ncodpers")
  } else {
    out <- fread("cleansed_test.csv", select=4) # ncodpers를 가져오자
  }
  for(product in product.list) {
    print(product)
    temp <- data[int_date %in% date.list[1:(i-1)], c("int_date","ncodpers",product), with=FALSE]
    temp <- temp[order(ncodpers, int_date)]
    temp$n00 <- temp$ncodpers==lag(temp$ncodpers) & lag(temp[[product]])==0 & temp[[product]]==0 # 상품 미보유(0) => 보유(0)
    temp$n01 <- temp$ncodpers==lag(temp$ncodpers) & lag(temp[[product]])==0 & temp[[product]]==1 # 상품 미보유(0) => 보유(1)
    temp$n10 <- temp$ncodpers==lag(temp$ncodpers) & lag(temp[[product]])==1 & temp[[product]]==0 # 상품 미보유(1) => 보유(0)
    temp$n11 <- temp$ncodpers==lag(temp$ncodpers) & lag(temp[[product]])==1 & temp[[product]]==1 # 상품 미보유(1) => 보유(1)
    temp[is.na(temp)] <- 0
    count <- temp[, .(sum(n00, na.rm=TRUE), sum(n01, na.rm=TRUE), sum(n10, na.rm=TRUE), sum(n11, na.rm=TRUE)), by=ncodpers]
    colnames(count)[2:5] <- paste0(product, c("_00","_01","_10","_11"))
    count[[paste0(product,"_0len")]] <- 0
    
    for(date in date.list[1:(i-1)]) {
      temp2 <- temp[int_date==date] 
      temp2 <- temp2[match(count$ncodpers, ncodpers)]
      flag <- temp2[[product]] == 0
      flag[is.na(flag)] <- 0
      count[[paste0(product,"_0len")]] <- (count[[paste0(product,"_0len")]] + 1) * flag # 최근 연속해서 그 상품을 보유하지 않은 날 길이를 구하는 코드
    }
    out <- merge(out, count, by="ncodpers")
  }
  write.csv(out[, -1, with=FALSE], paste0("count_", date.list[i], ".csv"), quote=FALSE, row.names=FALSE)
}
