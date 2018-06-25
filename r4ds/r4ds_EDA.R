library(tidyverse)

# To examine the distribution of a categorical variable, use a bar chart:
  
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
diamonds %>% 
  count(cut)

# To examine the distribution of a continuous variable, use a histogram:

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)
diamonds %>% 
  count(cut_width(carat, 0.5))
# graph can be different by how we let them draw
smaller <- diamonds %>% 
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

# If you wish to overlay multiple histograms in the same plot, I recommend using geom_freqpoly()
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# two centers
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50)) # zoom in by limiting y between 0 and 50    # and we can see some outliers
unusual <- diamonds %>% 
  filter(y < 3 | y > 20) %>% 
  select(price, x, y, z) %>%
  arrange(y)
unusual

# missing values
ggplot(diamonds %>% 
         filter(price < 2000))+geom_histogram(aes(price), binwidth = 100)

diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y)) # make outliers as NA

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE) # plot without NAs

nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time), # think if dep_time is NA, then it is highly possible that the flight was cancelled
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(nycflights13::flights, aes(dep_time)) + geom_bar()
ggplot(nycflights13::flights, aes(dep_time)) + geom_histogram()

# covariance
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +  # it was hard to consider number of each category, so use density now instead of count
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot() # freqpoly is sometimes hard to interpret, so use boxplot

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) # reordered by hwy median

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip() # flip

ggplot(data = mpg) +
  geom_lv(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) # lv

ggplot(data = mpg) +
  geom_violin(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# To visualise the covariation between categorical variables
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))
diamonds %>%  # another way and more common way
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

ggplot(data = diamonds) + # one way to solve overlap problem: alpha
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))

# hexa dots
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) #think continuous variable as categorical
# cut_width(x, width), as used above, divides x into bins of width

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))
# cut_number divide x into number of bins of number'


