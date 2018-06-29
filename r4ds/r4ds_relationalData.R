library(tidyverse)
library(nycflights13)

head(airlines)
head(airports)
head(planes)
head(weather)

#  If a table lacks a primary key, it’s sometimes useful to add one with mutate() and row_number()

flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

flights2 %>%
  select(-origin, -dest) %>% 
  mutate(name = airlines$name[match(carrier, airlines$carrier)]) # you can do this, but not recommended

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# inner join
x %>% 
  inner_join(y, by = "key")
# when there's duplicated keys(it doen't mean prime key), left or right or full(==outer) join is recommended
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")


# join by multiple keys
flights2 %>% 
  left_join(airports, c("dest" = "faa"))


#Filtering joins match observations in the same way as mutating joins, but affect the observations, not the variables. There are two types:
# semi_join(x, y) keeps all observations in x that have a match in y
# anti_join(x, y) drops all observations in x that have a match in y

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
flights %>% 
  filter(dest %in% top_dest$dest)
# this is same as...
flights %>%
  semi_join(top_dest)


df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
setdiff(df2, df1)













