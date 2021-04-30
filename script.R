#imports
#install.packages("ggplot2")
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(GGally)
library(leaps)
library(caret)
#set options
options(max.print = 10000)

#read data
fires <- read.csv(file = "forestfires.csv",header = TRUE, stringsAsFactors = TRUE)

#create column fire for future logistic/classification
#fires$fire <- rep("No",length(fires$X))
#fires$fire[fires$area>0] <-"Yes"

#set the new column as categorical
#fires$fire <- as.factor(fires$fire)

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

## try performance by removing collinearity
#drop_cols_coll <- names(fires) %in% c()
#fires_coll <- fires[!drop_cols_coll]
#lm.coll <- lm(log(fires_coll$area+1)~.,data=fires_coll)
#sqrt(vif(lm.coll))
#str(fires_coll)

lm.STM <- lm(log(area+1) ~ X + Y + day + month + temp + RH + 
               wind, data = fires_reg)

##Cross validation
#defining training control
set.seed(1)
train.control <- trainControl(method = "cv", number = 10)

#Train the models
model.big <- train(log(area+1) ~ .,data = fires_reg,method="lm", trControl = train.control)
model.STFWI <- train(log(area+1) ~ X + Y + day + month + FFMC + ISI + DMC + DC, data = fires_reg,method = "lm",trControl = train.control)
model.STM <- train(log(area+1) ~ X + Y + day + month + temp + RH + wind, data = fires_reg, method = "lm",trControl = train.control)
model.FWI <- train(log(area+1) ~ FFMC + ISI + DMC + DC, data = fires_reg,method = "lm",trControl = train.control)
model.M <- train(log(area+1) ~ temp + RH + wind, data = fires_reg,method = "lm",trControl = train.control)

##-------------------------------------------------------------------------------------------------------------------------------
##tests on multinomial logistic
fire_multinomial <- fires

#Convert month and day string variables into numeric values
fire_multinomial$month <- as.numeric(as.factor(fire_multinomial$month))
fire_multinomial$day <- as.numeric(as.factor(fire_multinomial$day))

#create the dataset with the categorical area
drop_area <- names(fires) %in% c("area")
fire_multinomial <- fire_multinomial[!drop_area]

#set the 6 levels
fire_multinomial$fire_class[fires$area == 0] <- 1
fire_multinomial$fire_class[fires$area>0 & fires$area<=4] <- 2
fire_multinomial$fire_class[fires$area>4 & fires$area<=40] <- 3
fire_multinomial$fire_class[fires$area>40 & fires$area<=120] <- 4
fire_multinomial$fire_class[fires$area>120 & fires$area<=400] <- 5
fire_multinomial$fire_class[fires$area>400] <- 6

#set the new column as a factor
fire_multinomial$fire_class <- factor(fire_multinomial$fire_class,levels = c(1,2,3,4,5,6), ordered = TRUE)

#build the ordered logic
glm.cl <- polr(fire_multinomial$fire_class ~  X + Y + day + month + temp + RH + wind, data = fire_multinomial)
summary(glm.cl)

#try the performance
set.seed(1)
# creating training data as 80% of the dataset
random_sample <- createDataPartition(fire_multinomial$fire_class, 
                                     p = 0.8, list = FALSE)
# generating training dataset and testing
# from the random_sample
training_dataset  <- fire_multinomial[random_sample, ]
testing_dataset <- fire_multinomial[-random_sample, ]

glm.cl <- polr(training_dataset$fire_class ~  X + Y + day + month + temp + RH + wind, data = training_dataset)
predictions <- predict(glm.cl, testing_dataset)


