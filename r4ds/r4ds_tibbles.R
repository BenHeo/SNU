# if want to know about tibbles, type   vignette("tibble")

library(tidyverse)
as_tibble(iris) # can change other types to tibbles by as_tibble
tibble(
  x = 1:5,
  y = 1,
  z = x^2 + y
)

tb <- tibble( # use backticks(``) to use colname which is not allowed in normal way
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb

# tribble : transposed tibble
tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)

# tibble shows restricted rows and columns. To make them show as much as I want use n & width
nycflights13::flights %>% 
  print(n = 10, width = Inf)

nycflights13::flights %>% 
  View() # such good~~~bb


# variable call
df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df$x
df[["x"]]
df[[1]]
# using pipe
df %>%
  .$x
df %>%
  .[["x"]]
