setwd("C:/Users/Abhi/Downloads")

ggplot(pseudoFacebook, aes(x = age))+
  geom_histogram(binwidth = 1, col = "blue", fill = '#F98765')+
  scale_x_continuous(breaks = seq(12,120,12) , lim = c(13,120))+
  labs(title = "Age of Facebook user", x = "Age of user", y = "Number of users")
 
by(pseudoFacebook$friend_count, pseudoFacebook$gender, summary)

pf<- read.csv("pseudo_facebook.tsv",sep = '\t')
library('ggplot2')
ggplot(subset(pf,!is.na(gender)), aes(x = friend_count)) +
  facet_grid(~gender)+
  geom_histogram(binwidth = 5, col = "blue", fill = '#F98765')+
  scale_x_continuous(limit = c(0,1000), breaks = seq(0,1000,30))

ggplot(subset(pf,!is.na(gender)), aes(x = tenure)) +
  facet_grid(gender~.)+
  geom_histogram(binwidth = 37, col = "blue", fill = '#F98765')+
  scale_x_continuous(limit = c(0,2555), breaks = seq(0,2555,365),minor_breaks = seq(183,2373,365),labels = c(0:7))+
  labs(x = "Öoooooga")

library('gridExtra')
p1 = ggplot(subset(pf, !is.na(gender)), aes(x = friend_count)) + geom_histogram(binwidth = 25, fill = '#F98765') + scale_x_continuous(breaks = seq(0,5000,250))
p2 = ggplot(subset(pf, !is.na(gender)), aes(x = friend_count + 1)) + geom_histogram(fill = '#F99865') + scale_x_log10()
p3 = ggplot(subset(pf, !is.na(gender)), aes(x = friend_count)) + geom_histogram(fill = '#F89865') + scale_x_sqrt()

grid.arrange(p1,p2,p3,ncol = 1)

ggplot(subset(pf,!is.na(gender)),aes(x = gender, y = friendships_initiated))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,500))

by(pf$friendships_initiated,pf$gender,summary)

##Exploring two variables

ggplot(aes(x = age, y = friend_count), data = pf)+
  geom_point(alpha = 1/20, position = position_jitter(h = 0), color = "orange")+
  coord_trans(y = "sqrt")+
  geom_line(stat = "summary", fun.y = mean)+
  geom_line(stat = "summary", linetype = 2, color = 'blue', fun.y = quantile, probs = 0.1)+
  geom_line(stat = "summary", linetype = 2, color = 'blue', fun.y = quantile, probs = 0.9)+
  geom_line(stat = "summary", color = 'blue', fun.y = quantile, probs = 0.5)+
  coord_cartesian(xlim = c(13,90), ylim = c(0, 2500))

cor(pf$age, pf$friend_count)
ggplot (aes(x = age, y = friendships_initiated), data = pf)+
  geom_point(alpha = 1/20, position = position_jitter(h = 0))+ ##Because jittering something with zero values could take it into negatives
  ## We do the above so that the jitter is only applied along the x axis and not along y. 
  xlim(13,90)+
  coord_trans(y = "sqrt")

install.packages('dplyr')
library(dplyr)

pf_fc_by_age <- pf %>%
  group_by(age)%>%
  summarise(friendcount_mean = mean(friend_count),
            friendcount_median = median(friend_count),
            n = n())%>%
  arrange(age)

head(pf_fc_by_age, 20)



ggplot(aes(x = likes_received, y = www_likes_received), data = pf)+
  geom_point(alpha = 0.05, color = "blue")+
  coord_cartesian(xlim = c(0,10000), ylim = c(0,10000))

with(pf, cor.test(likes_received, www_likes_received))

pf$age_in_months <- (pf$age * 12) + (12 - pf$dob_month)
pf$age_with_months <- (pf$age) + ((12-pf$dob_month)/12)

pf_fc_by_age_with_months <- pf %>%
  group_by(age_with_months)%>%
  summarise(friendcount_mean = mean(friend_count),
            friendcount_median = median(friend_count),
            n = n())%>%
  arrange(age_with_months)

c1<- ggplot(aes(x = age, y = friendcount_mean), data = subset(pf_fc_by_age, pf$age < 71)) +
  geom_line() +
  coord_cartesian(xlim = c(13,70), ylim = c(0,500))

c2 <- ggplot(aes(x = age_with_months, y = friendcount_mean), data = subset(pf_fc_by_age_with_months, age_with_months < 71)) +
  geom_line() +
  coord_cartesian(xlim = c(13,70), ylim = c(0,500))

library(gridExtra)
grid.arrange(c1, c2, ncol = 1)

library('dplyr')
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender))%>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n())%>%
  ungroup()%>%
  arrange(age)

head(pf.fc_by_age_gender)

ggplot (aes(x = age, y = median_friend_count), data = pf.fc_by_age_gender)+
  geom_line(aes(color = gender))

install.packages('reshape2')
library(reshape2)

pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender, age ~ gender, value.var = 'median_friend_count')
head(pf.fc_by_age_gender.wide)

library(ggplot2)
ggplot (aes(x = age, y = female/male ), data = pf.fc_by_age_gender.wide)+
  geom_line(aes(color = 'red'))+
  geom_hline(aes(yintercept = 1), linetype = 2)

pf$year_joined <- floor(2014 - (pf$tenure/365))
pf$year_joined.bucket <- cut(pf$year_joined, c(2004,2009,2011,2012,2014))

ggplot(aes(x = age, y = friend_count), data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

ggplot(aes(x = age, y = friend_count), data = subset(pf, !is.na(year_joined.bucket))) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)+
  geom_line(color = 'black', stat = 'summary', fun.y = mean)

pf$friending_rate <- pf$friend_count/pf$tenure

summary(subset(pf,tenure > 1)$friending_rate)

ggplot(aes(x = tenure, y = friendships_initiated/tenure), data = subset(pf, pf$tenure >= 1)) +
  geom_smooth(aes(color = year_joined.bucket))
  
install.packages('GGally')


