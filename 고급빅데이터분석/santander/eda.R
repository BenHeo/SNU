library(tidyverse)
library(data.table)
library(readr)
library(plotly)
# 
# santander <- fread('all/train_ver2.csv')
# naddasum <- data.frame(cnt = apply((santander == ""), 2, sum, na.rm = T)+apply(is.na(santander), 2, sum, na.rm =T))
# ggplot(naddasum, aes(x = rownames(naddasum), y = cnt)) + geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(size=10, angle=-45))

ncod <- santander %>% filter(is.na(age)) %>% select(ncodpers)
sum(!is.na(santander$age[santander$ncodpers %in% ncod$ncodpers]))
length(santander$ncodpers[which(!is.na(santander$age[santander$ncodpers %in% ncod$ncodpers]))])
length(unique(ncod$ncodpers))



#################################################################################################
df <- fread('cleansed_train.csv')
head(df)
summary(df)
apply(df[,20:43], 2, sum) # sum of products sold
apply(is.na(df), 2, sum)

df %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  rowSums(na.rm = TRUE) %>%
  which.max() # max sum is 15

product_sum <- as.data.frame(df %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  apply(2, sum))

names(product_sum) <- "cnt"
ggplot(product_sum, aes(x=rownames(product_sum), y = cnt)) + geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size=10, angle=-45)) + geom_label(aes(label = cnt))# + ylim(0, 2000000)

# less than 40000 times : ind_ahor_fin_ult1, ind_aval_fin_ult1, ind_cder_fin_ult1, ind_deco_fin_ult1, ind_deme_fin_ult1, ind_pres_fin_ult1
# less than 100000 times : ind_hip_fin_ult1, ind_viv_fin_ult1

quantile(product_sum$cnt)
product_sum[which(product_sum$cnt<100000),]


df %>%
  select(int_date, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  group_by(int_date) %>%
  summarise_all(sum) %>%
  gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  ggplot(aes(x=prods, y = bought, fill = as.factor(int_date))) + 
  geom_bar(stat = "identity", color = "black", position = "fill") +
  theme(axis.text.x = element_text(size=10, angle=-45))
# ind_deco_fin_ult1(단기예금)을 제외하면 1~5월 상품 구매 양과 6월~12월 상품 구매 양이 다르다. 1~5월에 많이 구매한다

df %>%
  select(int_date, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  group_by(int_date) %>%
  summarise_all(sum) %>%
  gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  ggplot(aes(x=as.factor(int_date), y = bought, fill = prods)) + 
  geom_bar(stat = "identity", color = "black", position = "fill") +
  theme(axis.text.x = element_text(size=10, angle=-45))


df %>%
  select(int_date, ind_deco_fin_ult1, ind_deme_fin_ult1, ind_dela_fin_ult1) %>%
  group_by(int_date) %>%
  summarise_all(sum) %>%
  gather(prods, bought, ind_deco_fin_ult1:ind_dela_fin_ult1) %>%
  ggplot(aes(x=as.factor(int_date), y = bought, color = prods, group = prods)) + 
  geom_line() +
  theme(axis.text.x = element_text(size=10, angle=-45)) + ylim(0, 2500)


# 각 월에 순수 추가된 상품 수만 고려
product_per_month <- df %>%
  select(int_date, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  group_by(int_date) %>%
  summarise_all(sum)
ggplotly(cbind(int_date = product_per_month$int_date[2:17] , product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date)) %>%
  gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  ggplot(aes(x=as.factor(int_date), y = bought, fill = prods)) + 
  geom_bar(stat = "identity", color = "black", position = "fill") +
  theme(axis.text.x = element_text(size=10, angle=-45)))
ggplotly(cbind(int_date = product_per_month$int_date[2:17] , product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date)) %>%
           gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
           ggplot(aes(x=as.factor(int_date), y = bought, fill = prods)) + 
           geom_bar(stat = "identity", color = "black") +
           theme(axis.text.x = element_text(size=10, angle=-45)))
ggplotly(cbind(int_date = product_per_month$int_date[2:17] ,
               (product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date))) %>%
           gather(prods, ratio, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
           ggplot(aes(x=as.factor(int_date), y = ratio, color = prods, group = prods)) + 
           geom_line() + theme(axis.text.x = element_text(size=10, angle=-45)))

# this month divided by last month
ggplotly(cbind(int_date = product_per_month$int_date[2:17] ,
               (product_per_month[2:17,] %>% select(-int_date) / product_per_month[1:16,] %>% select(-int_date))) %>%
           gather(prods, ratio, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
           ggplot(aes(x=as.factor(int_date), y = ratio, color = prods, group = prods)) + 
           geom_line() + theme(axis.text.x = element_text(size=10, angle=-45)))
# see lift
ggplotly(cbind(int_date = product_per_month$int_date[2:17] , (product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date))/product_per_month[1:16,] %>% select(-int_date)) %>%
           gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
           ggplot(aes(x=as.factor(int_date), y = bought, fill = prods)) + 
           geom_bar(stat = "identity", color = "black", position = "fill") +
           theme(axis.text.x = element_text(size=10, angle=-45)))
ggplotly(cbind(int_date = product_per_month$int_date[2:17] , (product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date))/product_per_month[1:16,] %>% select(-int_date)) %>%
           gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
           ggplot(aes(x=as.factor(int_date), y = bought, fill = prods)) + 
           geom_bar(stat = "identity", color = "black") +
           theme(axis.text.x = element_text(size=10, angle=-45)))

# ggplotly(cbind(int_date = product_per_month$int_date[2:17] , (product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date))/product_per_month[1:16,] %>% select(-int_date)) %>%
#            gather(prods, ratio, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
#            ggplot(aes(x=as.factor(int_date), y = ratio, color = prods, group = prods)) + 
#            geom_line() + theme(axis.text.x = element_text(size=10, angle=-45)))

cbind(int_date = product_per_month$int_date[2:17] , product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date)) %>%
  gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  ggplot(aes(x=prods, y = bought, fill = as.factor(int_date))) + 
  geom_bar(stat = "identity", color = "black") +
  theme(axis.text.x = element_text(size=10, angle=-45)) 

cbind(int_date = product_per_month$int_date[2:17] , product_per_month[2:17,] %>% select(-int_date) - product_per_month[1:16,] %>% select(-int_date)) %>%
  gather(prods, bought, ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  filter(prods == "ind_cder_fin_ult1") 

cumsum(1:26 * 3000)














# # discrete variables
# attach(santander)
# table(fecha_dato)
# table(ind_empleado)
# table(pais_residencia)
# table(sexo)
# table(ind_nuevo)
# table(santander$indrel_1mes)
# table(indrel)
# table(tiprel_1mes)
# table(indresi)
# table(indext)
# table(conyuemp)
# table(canal_entrada)
# table(indfall)
# table(tipodom)
# table(cod_prov)
# table(nomprov)
# table(ind_actividad_cliente)
# table(segmento)
# table(ind_ahor_fin_ult1)
# table(ind_aval_fin_ult1)
# table(ind_cco_fin_ult1)
# table(ind_cder_fin_ult1)
# table(ind_cno_fin_ult1)
# table(ind_ctju_fin_ult1)
# table(ind_ctma_fin_ult1)
# table(ind_ctop_fin_ult1)
# table(ind_ctpp_fin_ult1)
# table(ind_deco_fin_ult1)
# table(ind_deme_fin_ult1)
# table(ind_dela_fin_ult1)
# table(ind_ecue_fin_ult1)
# table(ind_fond_fin_ult1)
# table(ind_hip_fin_ult1)
# table(ind_plan_fin_ult1)
# table(ind_pres_fin_ult1)
# table(ind_reca_fin_ult1)
# table(ind_tjcr_fin_ult1)
# table(ind_valo_fin_ult1)
# table(ind_viv_fin_ult1)
# table(ind_nomina_ult1)
# table(ind_nom_pens_ult1)
# table(ind_recibo_ult1)
# detach(santander)

library(corrplot)
santander %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1) %>%
  colSums()

santander %>%
  group_by(ncodpers) %>%
  count() %>% View()

ggplot(santander, aes(x=age)) + geom_histogram() + xlim(0,100)
quantile(santander$age, na.rm = T)

ggplot(santander, aes(x=antiguedad)) + geom_histogram() + xlim(0, 6)




#############################################################################3
san_test <- fread('all/test_ver2.csv')
apply(is.na(san_test), 2, sum)
apply((san_test == ""), 2, sum)

length(unique(san_test$ncodpers))

###############################################################################
cleansed_san_train <- fread('cleansed_train.csv')
mm <- cleansed_san_train$int_date
user <- cleansed_san_train$ncodpers
prods <- cleansed_san_train %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1)
sumpds <- apply(prods, 1, sum)
mpds <- bind_cols(month = mm, user = user, sumpds = sumpds)
mpds %>%
  group_by(month, user) %>%
  summarise(cnt = sum(sumpds)) %>%
  filter(month == 6, cnt != 0)

##############################################################################
cleansed_san_test <- fread('cleansed_test.csv')

length(unique(cleansed_san_train$ncodpers))
length(unique(cleansed_san_test$ncodpers))

clprods <- cleansed_san_train %>%
  select(ind_ahor_fin_ult1:ind_recibo_ult1)
data.frame(rosum = apply(clprods, 1, sum)) %>%
  filter(rosum == 0) %>% nrow()

cleansed_san_train %>%
  group_by(ncodpers) %>%
  summarise(n = n())

cleansed_san_train %>%
  filter(sexo == "UNKNOWN") %>%
  group_by(ncodpers)
