#imports
#install.packages("ggplot2")
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(GGally)
library(leaps)
#set options
options(max.print = 10000)

#read data
fires <- read.csv(file = "forestfires.csv",header = TRUE, stringsAsFactors = TRUE)
fires$X <- as.factor(fires$X)
fires$Y <- as.factor(fires$Y)

#create column fire for future logistic/classification
fires$fire <- rep("No",length(fires$X))
fires$fire[fires$area>0] <-"Yes"

#set the new column as categorical
fires$fire <- as.factor(fires$fire)

#print the dataset first 10 lines and the structure
head(fires)
str(fires)

#plot the number of big fires vs small fires
attach(fires)
ggplot(fires,aes(x=fire))+
  geom_bar(fill = "steelblue")+ 
  xlab("Big Fire") + theme_minimal(base_size = 12)

#skewness of the target variable
area_plot <-ggplot(data = fires)+
  geom_histogram(aes(area),binwidth = 40)+
  theme_minimal(base_size = 11)+
  xlab("Burned area (in hectares)")

#applying log transformation to get a more gaussian like shape
log_area_plot<-ggplot(data = fires)+
  geom_histogram(aes(log(area+1)),binwidth = 0.3,fill = "steelblue")+
  theme_minimal(base_size = 11)+
  xlab("Ln(area +1)")   
  
grid.arrange(area_plot,log_area_plot,ncol=2)

#correlation study
#filtering the categorical variables
drop_vars <- names(fires) %in% c("X","Y","month","day","fire")
ggcorr(fires[!drop_vars], label=TRUE, label_size=4, label_color='black')

#plot of dc-dmc
plot(fires[,6:7])

#multiple linear regression: full model
complete.lm <- lm(log(area+1) ~ .,data = fires)
summary(complete.lm)

#plotting the most useful predictors for regression
leaps <- regsubsets(log(area+1) ~ .,data = fires[,-14], nbest = 1)
plot(leaps,scale = "adjr2")
summary(leaps)
