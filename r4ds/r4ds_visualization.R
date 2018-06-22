library(tidyverse)
head(mpg)
ggplot(mpg)+geom_point(mapping = aes(displ, hwy))

ggplot(mpg) # nothing is seen
nrow(mpg) # num of rows
ncol(mpg)

ggplot(mpg) + geom_point(aes(hwy, cyl))
ggplot(mpg) + geom_point(aes(class, drv)) # since class is nominal variable it isn't useful

ggplot(mpg) + geom_point(aes(displ, hwy, color = class))
ggplot(mpg) + geom_point(aes(displ, hwy, alpha = class))
ggplot(mpg) + geom_point(aes(displ, hwy, shape = class))
ggplot(mpg) + geom_point(aes(displ, hwy), color = "blue")

str(mpg)
summary(mpg)
ggplot(mpg) + geom_point(aes(displ, hwy, stroke = cyl/2))

ggplot(mpg) + geom_point(aes(displ, hwy, color = (displ<5))) # less than 5 vs. larger than 5

ggplot(mpg) + geom_point(aes(displ, hwy)) +
  facet_wrap(~ class, nrow = 2) # class에 맞춰서 여러 개로 분리해서 보여준다

ggplot(mpg) + geom_point(aes(displ, hwy))+
  facet_grid(drv~cyl) # 두 변수로 비
ggplot(mpg) + geom_point(aes(displ, hwy))+
  facet_grid(.~cyl) # 한 변수로만 비교하고 싶을 때







# point vs. smooth
p = ggplot(mpg)
p + geom_point(aes(displ, hwy))
p + geom_smooth(aes(displ, hwy))
p + geom_smooth(aes(displ, hwy, linetype = drv))

ggplot(mpg) + geom_point(aes(displ, hwy, color = drv)) + geom_smooth(aes(displ, hwy, color = drv))
ggplot(mpg, aes(displ, hwy, color = drv)) + geom_point() + geom_smooth() # exactly same code as above

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=class)) + geom_smooth()

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=class)) + geom_smooth(data = filter(mpg, class=="subcompact")) # 'data=' is necessary

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(show.legend = F) + 
  geom_smooth(se = FALSE, show.legend = F)

ggplot(diamonds) + geom_bar(aes(cut))
ggplot(diamonds) + stat_count(aes(cut)) # exactly same

demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
demo
ggplot(demo) + geom_bar(aes(cut, freq), stat = "identity")

ggplot(diamonds) + geom_bar(aes(cut, ..prop.., group = 1))
ggplot(diamonds, aes(cut, depth)) + geom_col() # what's difference??

ggplot(diamonds) + geom_bar(aes(cut, color=cut))
ggplot(diamonds) + geom_bar(aes(cut, fill=cut)) # it's more like what I wanted
ggplot(diamonds) + geom_bar(aes(cut, fill=clarity)) # it is stacked
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity") # it is not stacked. just seems stacked but overlapped
ggplot(data = diamonds, mapping = aes(x = cut, color = clarity)) + 
  geom_bar(fill = NA, position = "identity") # no fill, just color
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "fill") # fully fill each one
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge") # show distribution each
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(position = "jitter")  # some datas were overlapped but jitter makes them all seen by shaking


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_jitter()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_count() # size of dot increase according to same data

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()


# map data & flip
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()       

# polar cordinates
bar <- ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1 ) + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed() # 기울기 유지를 위해?



