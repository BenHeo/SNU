# 1
a_bef <- matrix(0, 1000, 1000)
a_bef[1:10, 1:10]
even <- seq(2,1000,2)
a_bef[even, even] <- 1
a_bef[1:10, 1:10]
a <- a_bef

# 2
my_csum <- function(vec){
  re_vec <- vec
  re_vec[1:length(re_vec)] <- 0
  my_cum <- 0
  for (i in 1:length(vec)){
    my_cum <- my_cum + vec[i]
    re_vec[i] <- my_cum
  }
  return(re_vec)
}
my_csum(1:5)

# 3
set.seed(1)
ru <- runif(1000)
my_cavg <- function(vec){
  re_vec <- vec
  re_vec[1:length(re_vec)] <- 0
  my_cum <- 0
  for (i in 1:length(vec)){
    my_cum <- my_cum + vec[i]
    my_avg <- my_cum/i
    re_vec[i] <- my_avg
  }
  return(re_vec)
}
re_avg <- my_cavg(ru)
x <- 1:1000
y <- re_avg[x]
plot(x, y, type = 'l')