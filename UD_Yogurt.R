yo <- read.csv('yogurt.csv')

library(ggplot2)

ggplot(aes(x = price), data = yo)+
  geom_histogram(binwidth = 5)+
  coord_cartesian(xlim = c(19,70))

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

ggplot(aes(x = time, y = price), data = yo) +
  geom_point(alpha = 0.2, color = 'orange')

set.seed(8765)

samples.ids <- sample(levels(yo$id),16)

ggplot(aes(x = time, y= price), data = subset(yo, id %in% samples.ids))+
  facet_wrap(~id)+
  geom_line()+
  geom_point(aes(size = all.purchases), pch = 1)