library(tidyverse)
library(data.table)
library(readr)
san_test <- fread('all/test_ver2.csv')
head(san_test)
nrow(san_test)

apply(is.na(san_test), 2, sum)
