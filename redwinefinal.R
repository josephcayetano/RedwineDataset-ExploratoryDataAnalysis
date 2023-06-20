# Univariate Plots Section

#loading data
yo <- read.csv('wineQualityReds.csv') 
library(ggplot2)
library(gridExtra)

#### Variables of wine dataset
names(yo)

#### Structure of wine dataset
str(yo)

#### Summary of dataset
summary(yo$quality)

#### Total count of each quality
table(yo$quality)

#### Quality Barchart
ggplot(aes(x = quality),data = yo)+
  geom_bar(color = 'black', fill = I('orange'))+
  scale_x_continuous(breaks = seq(3, 8 ,1)) + 
  ggtitle ("Barchart of quality")

#### Creating 'rating' variable
#### qualities 3-4 are rated C
#### qualities 5-6 are rated B
#### qualities 7-8 are rated A
yo$rating <- ifelse(yo$quality < 5, 'C', 
                    ifelse(yo$quality < 7, 'B', 'A'))

#ordering the 'rating' variable
yo$rating <- ordered(yo$rating,
                     levels = c('C', 'B', 'A'))

summary(yo$rating)

#quality barchart with rating legend
plot1<-ggplot(aes(x = quality, fill = rating),data = yo)+
  geom_bar(color = 'black')+
  ggtitle ("Barchart of quality")+
  scale_x_continuous(breaks = seq(3, 8, 1))

#rating barchart
plot2<-ggplot(aes(x = rating),data = yo)+
  geom_bar(color = 'black')+
  ggtitle ("Barchart of rating")

grid.arrange(plot1, plot2)

#Fixed acidity barchart
ggplot(data = yo, aes(x = fixed.acidity)) + 
  geom_histogram(binwidth = 0.2, color = 'black') + 
  scale_x_continuous(breaks = seq(4,16,.5)) +
  xlab("fixed acidity") + ggtitle ("Histogram of fixed acidity")

summary(yo$fixed.acidity)

#volatile acidity histogram
plot1<-ggplot(data = yo, aes(x = volatile.acidity)) + 
  geom_histogram(binwidth = 0.1, color = 'black', fill = I('orange')) + 
  scale_x_continuous(breaks = seq(0,1.6,.1)) +
  xlab("volatile acidity") + ggtitle ("Histogram of volatile acidity")

#volatile acidity with log transform histogram
plot2<-ggplot(data = yo, aes(x = volatile.acidity)) + 
  geom_histogram(binwidth = 0.3, color = 'black', fill = I('orange')) + 
  scale_x_log10() + xlab('volatile acidity') +
  ggtitle ("Log transform of volatile acidity")

grid.arrange(plot1, plot2)

summary(yo$volatile.acidity)

#### Citric acid histogram

ggplot(data = yo, aes(x = citric.acid)) + 
  geom_histogram(binwidth = .02, color = 'black', fill = I('blue')) +
  xlab('citric acid in grams') + ggtitle ("Histogram of citric acid")

summary(yo$citric.acid)

#residual sugar with log transform barchart
plot1<-ggplot(data = yo, aes(x = residual.sugar)) +
  geom_histogram(color = 'black', fill = I('orange'))+
  scale_x_log10()+ 
  ggtitle ("Log transform of residual sugar") +
  xlab("residual sugar in grams")

#residual sugar barchart
plot2<-ggplot(data = yo, aes(x = residual.sugar)) + 
  geom_histogram(binwidth = 0.3, color = 'black', fill = I('orange')) +
  scale_x_continuous(limits = c(0, quantile(yo$residual.sugar, 0.95)),
                     breaks = seq(0,8,.5)) + 
  ggtitle ("Histogram of residual sugar") +
  xlab("residual sugar in grams")

grid.arrange(plot2, plot1)

summary(yo$residual.sugar)

#chlorides histogram
plot1<-ggplot(data = yo, aes(x = chlorides)) + 
  geom_histogram(binwidth = .02, color = 'black') + 
  xlab('chlorides') + 
  ggtitle ("Histogram of chlorides")

#chlorides histogram with log transform
plot2<-ggplot(data = yo, aes(x = chlorides)) + 
  geom_histogram(color = 'black') +  scale_x_log10() +
  ggtitle ("Log transform of chlorides") +
  xlab('chlorides')

grid.arrange(plot1, plot2) 

summary(yo$chlorides)

#### Free sulfur dioxide histogram

ggplot(data = yo, aes(x = free.sulfur.dioxide)) + 
  geom_histogram(binwidth = 2, color = 'black', fill = I('green')) +
  ggtitle ("Histogram of free sulfur dioxide") +
  xlab('free sulfur dioxide in milligram')

summary(yo$free.sulfur.dioxide)

#total sulfur dioxide histogram
plot1<-ggplot(data = yo, aes(x = total.sulfur.dioxide)) + 
  geom_histogram(binwidth = 5, color = 'black') +
  ggtitle ("Histogram of total sulfur dioxide") +
  xlab('total sulfur dioxide in milligram')

#total sulfur dioxide with log transform histogram
plot2<-ggplot(data = yo, aes(x = total.sulfur.dioxide)) + 
  geom_histogram(color = 'black') + 
  scale_x_log10()+
  ggtitle ("Histogram of total sulfur dioxide with log transform") +
  xlab('total sulfur dioxide in milligram')

grid.arrange(plot1, plot2)

summary(yo$total.sulfur.dioxide)

#### Density histogram
ggplot(data = yo, aes(x = density)) + 
  geom_histogram(binwidth = .0002, color = 'black', fill = I('red')) +
  xlab('density in grams') + ggtitle ("Histogram of density")

summary(yo$density)

#### pH histogram

ggplot(data = yo, aes(x = pH)) + 
  geom_histogram(binwidth = .05, color = 'black', fill = I('yellow')) +
  xlab('pH') + ggtitle ("Histogram of pH")

summary(yo$pH)

#### Sulphates histogram

ggplot(data = yo, aes(x = sulphates)) + 
  geom_histogram(binwidth = .1, color = 'black', fill = I('orange')) +
  ggtitle ("Histogram of sulphates") +
  xlab('sulphates')

summary(yo$sulphates)

#### Alcohol barchart
ggplot(data = yo, aes(x = alcohol)) + 
  geom_histogram(binwidth = 0.3, color = 'black', fill = I('green')) + 
  xlab("% of alcohol by volume") + ggtitle ("Histogram of alcohol")

summary(yo$alcohol)

# Bivariate Plots Section

#### Boxplot of volatile acidity vs. rating and quality

#boxplot of volatile acidity vs rating
plot1<-ggplot(aes(x=rating,y = volatile.acidity), data = yo) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  coord_cartesian(ylim=c(0, quantile(yo$volatile.acidity, 0.99)))+
  geom_point(stat='summary',fun.y=mean,color='blue') +
  ggtitle ("Boxplot of volatile acidity vs. rating") +
  xlab('rating') + ylab('volatile acidity')

#bloxplot of volatile acidity vs quality
plot2<-ggplot(aes(x=factor(quality),y = volatile.acidity), data = yo) +
  geom_boxplot(color="red", fill="orange", alpha=0.2) +
  coord_cartesian(ylim=c(0, quantile(yo$volatile.acidity, 0.99)))+
  geom_point(stat='summary',fun.y=mean,color='blue')+
  xlab('quality')+
  geom_jitter(alpha=0.125,color='red') +
  ggtitle ("Boxplot of volatile acidity vs. quality") +
  xlab('quality') + ylab('volatile acidity')

grid.arrange(plot1, plot2)

#correlation test between volatile acidity and quality
cor.test(yo$volatile.acidity, yo$quality,method = 'kendall')

#### Boxplot of quality vs. alcohol

#creating 'q_groups' to collect the mean of some variables
library(dplyr)
q_groups <- group_by(yo, alcohol)
yo.q_groups <- summarise(q_groups,
                         volatile_mean = mean(volatile.acidity),
                         pH_mean = mean(pH),
                         sulphates_mean = mean(sulphates),
                         qmean=mean(quality),
                         n = n())
yo.q_groups <- arrange(yo.q_groups, alcohol)

#boxplot of alcohol vs quality
ggplot(aes(y=alcohol,x=factor(quality)),data = yo)+
  geom_boxplot(color="black", fill="red", alpha=0.2)+
  xlab('quality') + ylab('% of alcohol') +
  ggtitle ("Boxplot of alcohol vs. quality")

#correlation test between alcohol and quality
cor.test(yo$alcohol,yo$quality)

#### Does that mean that more alcohol gives us a better wine?

ggplot(aes(alcohol,qmean),data=yo.q_groups)+
  geom_smooth(color = 'red', fill = 'brown')+
  ylab("mean of quality")+
  scale_x_continuous(breaks = seq(0,15,0.5))+
  xlab("% of alcohol by volume") +
  ggtitle ("Lineplot of alcohol vs. mean of quality")

#creating 'up13' variable to collect wines with alcohol greater than 13
up13<-subset(yo,alcohol>13)

#correlation test between alcohol and quality above 13
cor.test(up13$quality,up13$alcohol)

#### Boxplot of quality vs. citric acid
ggplot(aes(y=citric.acid,x=factor(quality)),data=yo)+
  geom_boxplot(color="black", fill="violet", alpha=0.2)+
  xlab("quality") + ylab("citric acid in grams") +
  ggtitle ("Boxplot of citric acid vs. mean of quality")

#correlation test between citric acid and quality
cor.test(yo$citric.acid,yo$quality)

#### Boxplot of sulphates vs. quality
ggplot(aes(y=sulphates,x=factor(quality)),data=yo)+
  geom_boxplot(color="black", fill="green", alpha=0.2)+
  xlab("quality") + ylab("sulphates") +
  ggtitle ("Boxplot of sulphates vs. quality")

#correlation test between sulphates and quality
cor.test(yo$sulphates,yo$quality)

### View of quality through density plots

#### Density plot for alcohol vs. quality
ggplot(aes(x = alcohol, fill = factor(quality), color = factor(quality)), 
       data = yo) + geom_density(alpha=0.06) +
  xlab("% of alcohol by volume") +
  ggtitle ("Density plot of alcohol vs. quality")

#### Density plot for volatile acidity vs. quality
ggplot(aes(x = volatile.acidity, fill = factor(quality), color = factor(quality)), 
       data = yo) + geom_density(alpha=0.06) +
  xlab("volatile acidity") +
  ggtitle ("Density plot of volatile acidity vs. quality")

#### Density plot for sulphates vs. quality
ggplot(aes(x = sulphates, fill = factor(quality), color = factor(quality)), 
       data = yo) + geom_density(alpha=0.06) +
  xlab("sulphates") +
  ggtitle ("Density plot of sulphates vs. quality")

#### Density plot for citric acid vs. quality
ggplot(aes(x = citric.acid, fill = factor(quality), color = factor(quality)), 
       data = yo) + geom_density(alpha=0.06) +
  xlab("citric acid in grams") +
  ggtitle ("Density plot of citric acid vs. quality")

### pH relationship with fixed acidity and citric acid

#### Scatterplot of citric acid vs. pH
ggplot(aes(x = citric.acid, y = pH),data=yo)+
  geom_jitter(alpha=0.50, color = 'orange')+
  geom_smooth(method = 'lm', color = 'black') +
  xlab("citric acid in grams") +
  ggtitle ("Scatterplot of citric acid vs. pH")

#correlation test between pH and citric acid
cor.test(yo$pH,yo$citric.acid)

#### Scatterplot of fixed acidity vs. pH
ggplot(aes(x = fixed.acidity, y = pH),data=yo)+
  geom_jitter(alpha=0.50, color = 'brown')+
  geom_smooth(method = 'lm', color = 'black') +
  xlab("fixed acidity") +
  ggtitle ("Scatterplot of fixed acidity vs. pH")

#correlation test between fixed acidity and pH
cor.test(yo$fixed.acidity,yo$pH)

#### Scatterplot of free sulfur dioxide vs. total sulfur dioxide
ggplot(aes(y = free.sulfur.dioxide, x = total.sulfur.dioxide),data = yo) +
  geom_jitter(alpha=0.33,color='blue') +
  xlim(0,quantile(yo$total.sulfur.dioxide,0.99)) +
  xlab("total sulfur dioxide in milligram") + ylab("free sulfur dioxide in milligram") +
  ggtitle ("Scatterplot of total sulfur dioxide vs. free sulfure dioxide")

#correlation test between free sulfur dioxide and total sulfur dioxide
cor.test(yo$free.sulfur.dioxide, yo$total.sulfur.dioxide, method = 'pearson')

# Multivariate Plots Section

#### Scatterplot of alcohol vs. citric acid vs. quality
ggplot(data = yo, aes(x = alcohol, y = citric.acid)) +
  geom_point() + 
  geom_jitter(position = position_jitter(), aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette = 'Reds') +
  theme_dark() + xlab("% of alcohol") + 
  ylab("citric acid in grams") + 
  ggtitle ("Scatterplot of alcohol vs. citric acid vs. quality")

#### Scatterplot of alcohol vs. volatile acidity vs. quality
ggplot(data = yo, aes(x = alcohol, y = volatile.acidity)) +
  geom_jitter(alpha=3/4,position = position_jitter(), aes(color = factor(quality))) +
  scale_color_brewer(type='seq',palette = 'Oranges') +
  theme_dark() + xlab("% of alcohol") +
  ylab("volatile acidity") +
  ggtitle ("Scatterplot of alcohol vs. volatile acidity vs. quality")

#### Scatterplot of volatile acidity vs. citric acid vs. quality
ggplot(data = yo, aes(x = volatile.acidity, y = citric.acid)) +
  geom_point() + 
  geom_jitter(position = position_jitter(), aes(color = factor(quality))) +
  scale_color_brewer(type='qual') +
  geom_vline(xintercept=c(0.8),linetype='dashed', color = 'red')+
  ggtitle("Scatterplot of volatile acidity vs. citric acid vs. quality") +
  ylab("citric acid in grams") + xlab("volatile acidity")

#### Scatterplot of alcohol vs. sulphates vs. quality
ggplot(data = subset(yo, rating == 'A'|rating == 'C'), aes(x = alcohol, y = sulphates)) +
  geom_jitter(position = position_jitter(), aes(color = factor(quality))) +
  scale_color_brewer(type='div', palette = 2) +
  scale_y_continuous(breaks = seq(0,2,.2)) + 
  theme_dark() +
  ggtitle("Scatterplot of alcohol vs. sulphates vs. quality") +
  ylab("sulphates") + xlab("% of alcohol") +
  geom_hline(yintercept=c(0.6), linetype='dashed', color = 'blue', size = 1)

# Final Plots and Summary

### Plot one

#quality and rating barchart comparison
#quality barchart with rating legend
plot1<-ggplot(aes(x = quality, fill = rating),data = yo)+
  geom_bar(color = 'black')+
  ggtitle ("Barchart of quality")+
  scale_x_continuous(breaks = seq(3, 8, 1))+
  xlab("quality") + ylab("count")

#rating barchart
plot2<-ggplot(aes(x = rating),data = yo)+
  geom_bar(color = 'black')+
  ggtitle ("Barchart of rating") +
  xlab("rating") + ylab("count")

grid.arrange(plot1, plot2)

### Plot two

#boxplot of alcohol vs quality
plot1<-ggplot(aes(y=alcohol,x=factor(quality)),data = yo)+
  geom_boxplot(color="black", fill="red", alpha=0.2)+
  xlab('quality') + ylab('% of alcohol') +
  ggtitle ("Boxplot of alcohol vs. quality")

plot2<-ggplot(aes(x=alcohol,y=qmean),data=yo.q_groups)+
  geom_smooth(color = 'red', fill = 'brown')+
  ylab("mean of quality")+
  scale_x_continuous(breaks = seq(0,15,0.5))+
  xlab("% of alcohol by volume") +
  ggtitle ("Lineplot of alcohol vs. mean of quality")

grid.arrange(plot1, plot2)

### Plot three

##scatterplot of alcohol vs citric acid vs quality
ggplot(data = yo, aes(x = alcohol, y = citric.acid)) +
  geom_point() + 
  geom_jitter(position = position_jitter(), aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette = 'Reds') +
  theme_dark() + xlab("% of alcohol") + 
  ylab("citric acid in grams") + 
  ggtitle ("Scatterplot of alcohol vs. citric acid vs. quality")