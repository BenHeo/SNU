library(tidyverse)

# “Tidy datasets are all alike, but every messy dataset is messy in its own way.” –– Hadley Wickham

table1
table2
table3
table4a
table4b


## spread & gather
# One variable might be spread across multiple columns.
table4a %>%
  gather(`1999`, `2000`, key="year", value="cases")

table4b %>%
  gather(`1999`, `2000`, key="year", value = "populations")


left_join(table4a, table4b)

# One observation might be scattered across multiple rows.
table2 %>%
  spread(key = type, value = count)


stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  spread(year, return) %>% 
  gather("year", "return", `2015`:`2016`)


people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people %>% 
  spread(key, value) # maybe because of no keep column?

people <- tribble(
  ~name,             ~key,    ~value, ~obs,
  #-----------------|--------|------|------
  "Phillip Woods",   "age",       45, 1,
  "Phillip Woods",   "height",   186, 1,
  "Phillip Woods",   "age",       50, 2,
  "Jessica Cordero", "age",       37, 1,
  "Jessica Cordero", "height",   156, 1
)
spread(people, key, value)

preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)
gather(preg, pregnant, count)


## separate & unite
table3 %>% 
  separate(rate, into = c("cases", "population")) # default sep is non-alphanumeric character # you can use sep = "blah blah" 

table3 %>% 
  separate(rate, into = c("cases", "population"), convert = TRUE) # convert change col_type. If convert == FALSE, it is same as column before separated

table3 %>% 
  separate(year, into = c("century", "year"), sep = 2) # split into first two digits and else

table5 %>% 
  unite(new, century, year) # default sep is "_"

table5 %>%
  unite(new, century, year, sep = "", remove = F) # default is remove = TRUE


## Missing values  ------- explicitly or implicitly missing values
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>% 
  spread(year, return) %>% 
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>%
  complete(year, qtr) # it feels like cartesian multiple


treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment %>%
  fill(person) # fill by most recently used value

# practice with who dataset
head(who)
who1 <- who %>%
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
who1 %>%
  count(key)

# The first three letters of each column denote whether the column contains new or old cases of TB. 
#     In this dataset, each column contains new cases.

# The next two letters describe the type of TB

# The sixth letter gives the sex of TB patients

# The remaining numbers gives the age group


unique(who1$key) # newrel should be transformed into new_rel
who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2  

who3 <- who2 %>%
  separate(key, c("new", "type", "sexAge"), sep = "_")
who3
who3 %>%
  count(new)

who4 <- who3 %>%
  select(-new, -iso3, -iso2) # unselect redundance
who5 <- who4 %>%
  separate(sexAge, c("sex", "age"), sep = 1)
who5

# summarise
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)


# https://simplystatistics.org/2016/02/17/non-tidy-data/