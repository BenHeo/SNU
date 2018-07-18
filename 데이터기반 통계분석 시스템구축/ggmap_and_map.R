library(tidyverse)
library(maps)
if(!require(mapdata)){install.packages("mapdata") ;library(mapdata)}
# USA map
old_par <- par()
par(mfrow = c(1,2))
map(database = "usa") # lines
map(database = "county") # lines groupby county
# South Korea
map(database = "world", region = "South Korea")
map("world2Hires", "South Korea") # Higher resolution
data(us.cities)
head(us.cities)
map("state", "GEORGIA")
map.cities(us.cities, country = "GA")

par() <- old_par
map('world', fill = TRUE, col = rainbow(30))
