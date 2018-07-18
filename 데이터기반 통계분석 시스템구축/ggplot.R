library(tidyverse)
head(msleep)
ggplot(msleep, aes(bodywt, sleep_total)) # inform which data to use
ggplot(msleep) + geom_point(aes(bodywt, sleep_total)) # let's transform
ggplot(msleep) + geom_point(aes(log(bodywt), sleep_total)) # let's use color
ggplot(msleep) + geom_point(aes(log(bodywt), sleep_total, color = vore)) # color by vore. Let's separate
ggplot(msleep) + geom_point(aes(log(bodywt), sleep_total, color = vore)) + facet_grid(~vore) # use many canvas about vore
scatterplot <- ggplot(msleep, aes(log(bodywt), sleep_total, color = vore))
ScatterPlot <- scatterplot + geom_point(size = 5) + 
  xlab('Log Body Weight') + ylab('Total Sleep Hours') + ggtitle("Some Sleep Data")
ScatterPlot

ggplot(msleep, aes(vore, sleep_total)) + geom_point()
ggplot(msleep, aes(vore, sleep_total, color = vore)) + geom_jitter()
ggplot(msleep, aes(vore, sleep_total, color = vore)) + geom_jitter(position = position_jitter(width = 0.2), # jitter in 0.2 width
                                                                   size = 5, alpha = 0.5) # size of dot is 5, capacity 0.5

dane <- data.frame(mylevels = c(1,2,5,9), myvalues = c(2,5,3,4))
head(dane)
ggplot(dane, aes(factor(mylevels), myvalues)) + geom_line(group = 1) + geom_point(size=3) # geom_line draw line by group
ggplot(dane, aes(factor(mylevels), myvalues)) + geom_line(group = c(1,1,2,2)) + geom_point(size=3) 

data("economics"); head(economics)
data("presidential"); head(presidential)
ggplot(economics, aes(date, unemploy)) + geom_line() # let's use presidential info
president = subset(presidential, start > economics$date[1])
ggplot(economics) + geom_rect(aes(xmin=start, xmax=end, fill = party), ymin=-Inf, ymax=Inf, data = president) +
  geom_line(aes(date, unemploy), data = economics)
ggplot(economics) + geom_rect(aes(xmin=start, xmax=end, fill = party), ymin=-Inf, ymax=Inf, data = president, color = "black") +
  geom_line(aes(date, unemploy), data = economics) # black color show when president changed

library(datasets)
data("airquality")
aq_trim <- airquality[which(airquality$Month == 7 | airquality$Month == 8 | airquality$Month == 9),]
aq_trim$Month <- factor(aq_trim$Month, labels = c("July", "August", "September"))
ggplot(aq_trim, aes(Day, Ozone, size = Wind, fill = Temp)) + geom_point(shape = 21) + 
  ggtitle("Air Quality in New York by Day") + labs(x = "Day of the Month", y = "Ozone(ppb)") +
  scale_x_continuous(breaks = seq(1, 31, 5)) # xlab word writing point

library(reshape2)
festival.data <- read.table("data/DownloadFestival.dat", sep = '\t', header = T)
head(festival.data) # day data is spreaded(wide format) ==> change to long format to use histogram

day1Hist <- ggplot(festival.data, aes(day1));
day1Hist + geom_histogram(color='royalblue1', fill = 'royalblue2', binwidth = 0.1)
day1Hist + geom_histogram(color = 'royalblue3', fill = 'yellow', bins = 35, binwidth = 0.2, aes(y=..density..)) # not freq, y is density
                                                                  # bins is number of bars
day1Hist + geom_histogram(binwidth = 0.1, aes(y=..density..), color = "black", fill = "lightblue") +
  geom_density(alpha = 0.2, fill = '#FF6666') # density area

festival.data.stack <- melt(festival.data, id = c('ticknumb', 'gender'))
colnames(festival.data.stack)[3:4] = c("day", "score")

Histogram.3day2 <- ggplot(festival.data.stack, aes(score)) +
  geom_histogram(binwidth = 0.4, color = "black", fill = "yellow") + 
  labs(x = "Score", y = "Counts")
Histogram.3day2 + facet_grid(~gender)
Histogram.3day2 + facet_grid(gender~day) # we want density comparison
Histogram.3day2 <- ggplot(festival.data.stack, aes(score, ..density..)) +
  geom_histogram(binwidth = 0.4, color = "black", fill = "yellow") + 
  labs(x = "Score", y = "Counts")
Histogram.3day2 + facet_grid(~gender)
Histogram.3day2 + facet_grid(gender~day) # It is good for skewness, kurtosis, otherwise use better plot

Scatterplot <- ggplot(festival.data.stack, aes(x=gender, y=score, color=gender)) +
  geom_point(position = "jitter") + facet_grid(~day)
Scatterplot
Scatterplot + scale_color_manual(values = c('darkorange', 'darkorchid4'))
Scatterplot + geom_boxplot(alpha=0.2, color='black', fill='orange')
