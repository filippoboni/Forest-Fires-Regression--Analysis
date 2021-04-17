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

#create column fire for future logistic/classification
#fires$fire <- rep("No",length(fires$X))
#fires$fire[fires$area>0] <-"Yes"

#set the new column as categorical
#fires$fire <- as.factor(fires$fire)

#Convert month and day string variables into numeric values
#fires$month <- as.numeric(as.factor(fires$month))
#fires$day <- as.numeric(as.factor(fires$day))

#visualize the variables distributions
par(mfrow=c(3,4))
hist(fires$area,100,main = "area",xlab = "")
hist(fires$X,100,main = "X",xlab = "")
hist(fires$Y,100,main = "Y",xlab = "")
hist(fires$FFMC,100,main = "FFMC",xlab = "")
hist(fires$DMC,100,main = "DMC",xlab = "")
hist(fires$DC,100,main = "DC",xlab = "")
hist(fires$ISI,100,main = "ISI",xlab = "")
hist(fires$temp,100,main = "temp",xlab = "")
hist(fires$RH,100,main = "RH",xlab = "")
hist(fires$wind,100,main = "wind",xlab = "")
hist(fires$rain,70,main = "rain",xlab = "")

#plot the number of big fires vs small fires
attach(fires)
ggplot(fires,aes(x=fire))+
  geom_bar(fill = "steelblue")+ 
  xlab("Big Fire") + theme_minimal(base_size = 12)

##correlation study
#filtering the categorical variables
drop_vars <- names(fires) %in% c("month","day","rain")
ggcorr(fires[!drop_vars], label_size=4, label_color='black')

##processing
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

#set X,Y as dummy variables
fires$X <- as.factor(fires$X)
fires$Y <- as.factor(fires$Y)

#drop rain variable and define the dataset for regression
drop_cols <- names(fires) %in% c("rain")
fires_reg <- fires[!drop_cols]
attach(ffires_reg)
head(fires_reg)

##Regression models
#simple linear model
lm.simple <- lm(log(area+1) ~ temp,data = fires_reg)
summary(lm.simple)

#multiple linear regression: full model
complete.lm <- lm(log(area+1) ~ .,data = fires)
summary(complete.lm)

#plotting the most useful predictors for regression
leaps <- regsubsets(log(area+1) ~ .,data = fires[,-14], nbest = 1)
plot(leaps,scale = "adjr2")
summary(leaps)


