setwd('C:/Users/Abhi/Downloads')

rw <- read.csv('wineQualityReds.csv')

library(ggplot2)
library(GGally)

theme_set(theme_minimal(20))

set.seed(1997)
rw_subset<- rw[,c(2:13)]

ggpairs(rw_subset)

#fixed acidity vs citric acid
# fixed acidity vs (density or ph)
# citric acid vs (density or ph)
# free vs total suplhur
#Citric vs volatile acidity

ggplot(aes(alcohol,quality, color = volatile.acidity), data = rw)+
  geom_point()

ggplot(aes(volatile.acidity), data = rw)+
  geom_histogram()+
  scale_x_log10()

ggplot(aes(residual.sugar), data = rw)+
  geom_histogram()+
  scale_x_log10()

ggplot(aes(alcohol), data = rw)+
  geom_histogram()+
  scale_x_log10()

rw$quality <- factor(rw$quality)

ggplot(aes(sulphates,alcohol,color = quality), data = rw)+
  geom_point()+
  scale_color_gradientn(colors = c('#ffff00','#ffbf00','#ff9999','#9999ff','#333399','#000033'))+
  scale_x_continuous(trans = log_trans())+
  scale_y_continuous(trans = log_trans())

ggplot(aes(alcohol,quality, color = volatile.acidity), data = subset(rw,rw$volatile.acidity < quantile(rw$volatile.acidity, 0.99)))+
  geom_point(position = 'jitter', alpha = 0.8, size = 2)+
  scale_color_gradientn(colors = c('#ffff00','#ffbf00','#ff9999','#9999ff','#333399','#000033'))+
  coord_cartesian(xlim = c(quantile(rw$alcohol,0.01),quantile(rw$alcohol,0.99)))

ggplot(aes(quality, volatile.acidity), data = rw)+
  geom_point(alpha = 0.4, position = 'jitter')

ggplot(aes(alcohol, quality), data = rw)+
  geom_point(alpha = 0.4, position = 'jitter')

mod1 <- lm(quality ~ alcohol, data = rw)
mod2 <- update(mod1, ~ . + sulphates)

summary(rw)
