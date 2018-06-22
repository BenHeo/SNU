library(nycflights13)
library(tidyverse)
head(flights)
# dplyr package
# filter(), arrange(), select(), mutate(), summarise(), group_by()
jan1 <- filter(flights, month == 1, day == 1)
(dec25 <- filter(flights, month == 12, day == 25)) # with parenthesis, print and save both work
near(sqrt(2)^2, 2) # computer is not good at finite numbers, so near function solves the problem by using epsilon
nov_dec <- filter(flights, month %in% c(11,12))
dfna <- tibble(x = c(1,NA,3))
filter(dfna, x>1) # filter only returns TRUE value

arrange(flights, year, month, day)
arrange(flights, desc(dep_delay))
arrange(dfna, x); arrange(dfna, desc(x)) # NA is always last

select(flights, year, month, day)
select(flights, year:day) # : is very efficient
select(flights, -(year:day))
# select has many sub functions-----starts_with("abc"), ends_with("xyz"), contains("ijk"), matches("(.)\\1"), num_range("x", 1:3)
# select also has rename function, but it is recommended to use it separately like rename(flights, tail_num = tailnum)
select(flights, time_hour, air_time, everything()) #### booooooom! powerful

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
                      )
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance/air_time * 60
       )
# View(flights_sml) # if you want to see all variables it is better than seeing console
transmute(flights_sml, 
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain/hours
          ) # when you want to use only new variables 

x <- 1:10; x
lag(x)
lead(x)
# cumsum(), cumprod(), cummin(), cummax(), cummean()
# min_rank(), row_number(), dense_rank(), percent_rank()

select(flights, dep_time, sched_dep_time) %>%
  filter(dep_time <= 602, dep_time > 558)

select(flights, dep_time, sched_dep_time) %>%
  transmute(dep_hour = dep_time %/% 100,
            dep_min = dep_time %% 100,
            sched_hour = sched_dep_time %/% 100,
            sched_min = sched_dep_time %% 100) %>%
  transmute(dep = as.Date(paste0(dep_hour,":", dep_min), "%H:%M"),
            sched = as.Date(paste0(sched_hour, ":", sched_min), "%H:%M"))  ## needs to solve this.... I want time form, not date form

summarise(flights, delay = mean(dep_delay, na.rm = T))
flights %>%
  group_by(year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm=T)) # summarise is useful with group_by function!

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(count > 20, dest != "HNL")
ggplot(delays, aes(dist, delay)) + geom_point(aes(size = count), alpha = 1/3) + geom_smooth(se=FALSE)


not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    n = n(),
    delay = mean(arr_delay, na.rm = T)
  )
ggplot(delays, aes(n, delay)) + geom_point(alpha = 1/10)
delays %>%
  filter(n > 25) %>%
  ggplot(aes(n,delay)) + geom_point(alpha = 1/10)

# useful summarise functions : mean(), median(), sd(), IQR(), mad() --- mean absolute deviation, min(), quantile(), max(), 
#                               first(), nth(x, 2), last(), n_distinct(), count(), count(tailnum, wt = distance) --- count tailnum with weight distance

# When you group by multiple variables, each summary peels off one level of the grouping. 
# That makes it easy to progressively roll up a dataset:
  
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)


popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
