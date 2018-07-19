library(tidyverse)
library(maps)
if(!require(mapplots)){install.packages("mapplots") ;library(mapplots)}
if(!require(ggmap)){install.packages("ggmap") ;library(ggmap)}
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

# unemployment rate map
data(unemp)
data(county.fips)
head(unemp,3)
head(county.fips, 3)

# give color
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)] # county.fips의 fips factor와 unemp$fips를 맞추기 위해
colors = c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0","#DD1C77","#980043")
if(!require(mapproj)){install.packages("mapproj") ;library(mapproj)} # 구면좌표계를 잘 표현하기 위해
map("county", col = colors[colorsmatched], fill = TRUE,
    resolution = 0, lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, # add = TRUE를 이용해 주 나누기
    lwd = 0.2,projection = "polyconic")
title("unemployment by county, 2009")

wm <- ggplot2::map_data('world')
str(wm)
head(wm, 30) # each country(region) has own group number
ur <- wm %>% dplyr::select(region) %>% unique()
# group(polygon) 번호가 정확하게 나라를 나누는 것은 아니여서 region(국가명)으로 나라를 나누는 것이 정확하다
grep("Korea", ur$region) # 125, 185
grep("China", ur$region) # 42
grep("Japan", ur$region) # 116
map("world", ur$region[c(125,185)],fill = T,
    col = "blue")
map("world", ur$region[c(125, 185)], fill = T, col = rainbow(4)) # wrong map. Use country name not polygon
wmk <- wm %>%
  filter(region == "South Korea"| region == "North Korea")
wms <- wm %>%
  filter(region == "South Korea")
wmn <- wm %>%
  filter(region == "North Korea")
unique(wmk$group)
unique(wms$group)
unique(wmn$group)
map("world", ur$region[c(125, 185)], fill = T, col = c(rep("darkblue", 11), rep("pink", 2)))
map("world", ur$region[c(125, 185, 116)], fill = T, col = c(rep("red", 34), rep("darkblue", 11), rep("pink", 2) ))


map("worldHires", "South Korea")
seoul_loc = geocode("seoul")
busan_loc = geocode("Busan")
suwon_loc = geocode("suwon")
add.pie(z=1:2, labels=c('a','b'), 
        x = seoul_loc$lon, y = seoul_loc$lat, radius = 1)
add.pie(z=3:2, labels=c('a','b'), 
        x = busan_loc$lon, y = busan_loc$lat, radius = 0.5)
