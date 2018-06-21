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

